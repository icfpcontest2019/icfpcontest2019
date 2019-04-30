package lambda.geometry.floating.triangles

import lambda.geometry.floating.{FPoint, FPolygon}

/**
  * @author Ilya Sergey
  */

object DualGraphUtils {

  type DualGraph = Map[Triangle, Set[Triangle]]

  def overlapByAnEdge(t: Triangle, s: Triangle): Boolean = {
    if (t =~= s) return false
    for (e <- t.edges;
         d <- s.edges) {
      if (e =~= d || e.flip =~= d) {
        return true
      }
    }
    false
  }

  /**
    * Constructs a graph of a triangle set
    */
  def constructDualGraph(ts: Set[Triangle]): DualGraph = {
    def updateGraph(g: DualGraph, t: Triangle): DualGraph = {
      // Compute adjacent triangles
      val adjacent = ts.filter(overlapByAnEdge(t, _))
      g + (t -> adjacent.toSet)
    }

    ts.foldLeft(Map.empty: DualGraph)(updateGraph)
  }

  object NodeColor extends Enumeration {
    type NodeColor = Value
    val White, Gray, Black = Value
  }

  import NodeColor._

  type NodeColoring = Map[FPoint, NodeColor]

  private def dfsWorker(g: DualGraph, parent: Triangle, colors: NodeColoring, processed: Set[Triangle]): NodeColoring = {

    val pr = processed + parent

    def processChild(c: Triangle, col: NodeColoring): NodeColoring = {
      val vs = c.vertices.filterNot(v => parent.vertices.exists(_ =~= v))
      assert(vs.size == 1)
      // A new node to color
      val v = vs.head
      // take colors of existing nodes
      val presentColors = col.keys.filter(z => c.vertices.exists(_ =~= z)).map(col(_)).toSet
      assert(presentColors.size == 2)
      val newColor = complementColor(presentColors)
      val newColors = col + (v -> newColor)
      dfsWorker(g, c, newColors, pr)
    }

    val children = g.getOrElse(parent, Set.empty).filterNot(t => pr.exists(_ =~= t))
    children.foldLeft(colors)((clz, t) => processChild(t, clz))
  }

  def traverseAndColor(g: DualGraph): NodeColoring = {
    val ts = g.keys.toSeq
    assert(ts.nonEmpty)
    val root = ts.head
    // Initial coloring
    val colors: NodeColoring = Map(root.v1 -> White, root.v2 -> Gray, root.v3 -> Black)
    dfsWorker(g, root, colors, Set.empty)
  }

  def constructColoring(p: FPolygon): NodeColoring = {
    val ts = Triangulation.triangulate(p)
    val g = constructDualGraph(ts)
    val colors = traverseAndColor(g)
    colors
  }

  def complementColor(s: Set[NodeColor]): NodeColor = {
    assert(s.size == 2, s"Something wrong with color set $s")
    if (s == Set(Black, White)) Gray
    else if (s == Set(Gray, White)) Black
    else White
  }

  def chavatalVisibilitySet(p: FPolygon): Seq[FPoint] = {
    val colors = constructColoring(p)
    val byColor: Map[NodeColor, NodeColoring] = colors.groupBy(_._2)
    val subSets = byColor.map(_._2.keySet).toSeq
    assert(subSets.nonEmpty)
    val init = subSets.head
    val res = subSets.tail.foldLeft(init)((z, s) => if (s.size < z.size) s else z)
    res.toSeq
  }

}
