package lambda.geometry.floating.generators

import lambda.geometry.floating.FPolygon

import scala.collection.immutable.Queue

/**
  * @author Ilya Sergey
  */
object PolygonCombinatorUtils {

  implicit def _pcomb2Polygon(p: CompositePolygon): FPolygon = p.pol

  type ATree = Map[CompositePolygon, Set[Attached]]

  private def computePolygonTreeStep(acc: ATree, pc: CompositePolygon): ATree = pc match {
    case BasePolygon(_) => acc
    case z: Attached =>
      val par = z.parent
      assert(par.edges.exists(_ =~= z.e))
      val res = acc.getOrElse(par, Set.empty)
      // add z to the children of par
      acc.updated(par, res + z)
  }

  /*
   * Compute the map of children, representing the dependency tree.
   *
   * This is a crucial structure as it capture the causality between the polygons, as they were constructed.
   * By traversing it in a DFS or BFS-like manner, we can compile a reasonably no_need_obstacles set of intermediate-ishœ polygons
   *
   */
  def computeAttachmentTree(pc: CompositePolygon): ATree =
    pc.toList.foldLeft(Map(findBasic(pc) -> Set.empty): ATree)(computePolygonTreeStep)

  private def findBasic(pc: CompositePolygon): BasePolygon = pc match {
    case z@BasePolygon(pol) => z
    case Attached(base, e, attached) => findBasic(base)
  }

  // compute the tree of polygons given a map
  private def polygonQueueWorker(bp: CompositePolygon, map: ATree): Queue[CompositePolygon] = {
    // Basic polygon should be present as a key

    def bfsLoop(q: Queue[CompositePolygon],
                current: Seq[CompositePolygon]): Queue[CompositePolygon] = {
      val newChildren = for (par <- current; child <- map.getOrElse(par, Set.empty)) yield child
      if (newChildren.isEmpty) return q

      assert(newChildren.forall(!q.toSet.contains(_)))

      // Check that neither of the children is in the queue yet
      val children = newChildren
      bfsLoop(q ++ children, children)

    }
    assert(map.keySet.contains(bp))
    val initQueue = Queue(bp)
    bfsLoop(initQueue, Seq(bp))
  }

  /**
    * Construct the queue of increasingly complex polygons to reproduce the failing test (BFS)
    *
    * The problem is that so far it's a linear queue, re-tracing the construction
    *
    * How about we sprinkle it with new elements by combining fresh sub-branches? Precumably, we can do it just by
    * traversing the queue and "recompiling" the prefixes
    *
    */
  def polygonQueue(base: BasePolygon, tree: ATree, size: Int): List[CompositePolygon] = {
    polygonQueueWorker(base, tree).filter(_.size < size).toList
  }

  private def polygonStackWorker(map: ATree, s: List[CompositePolygon],
                                 cur: CompositePolygon): List[CompositePolygon] = {

    assert(!s.contains(cur), "Each polygon is added only once!")

    val children = map.getOrElse(cur, Set.empty)
    children.foldLeft(cur :: s) { case (st, pc) => polygonStackWorker(map, st, pc) }
  }

  def polygonStack(base: BasePolygon, tree: ATree, size: Int): List[CompositePolygon] = {
    polygonStackWorker(tree, Nil, base).reverse.filter(_.size < size)
  }

  /**
    * Return the base polygon and the tree based on it.
    */
  def getBaseTree(pc: CompositePolygon): ATree = {
    val base = findBasic(pc)
    val map = computeAttachmentTree(pc)
    assert(map.keySet.contains(base), s"$pc\n$base\n$map")
    map
  }


  def computeAllTrees(pc: CompositePolygon): Seq[(BasePolygon, ATree)] = {
    val tree = getBaseTree(pc)
    // compute meaningful subtrees
    (for {
      k <- tree.keys
      v = tree.getOrElse(k, Set.empty)
      (b, vNew) = k match {
        case z@BasePolygon(pol) => (z, v)
        case Attached(p, _, attached) => {
          val b = BasePolygon(attached)
          // take only those children that are properly attached to the new block
          // but not to the edges, causes by division of old ones
          val vNew = v.filter(p => b.pol.edges.exists(_ =~= p.e))
          (b, vNew)
        }
      }
    } yield (b, tree - k + (b -> vNew))).toSeq
  }

  /**
    * Fold a sequence of combinators into a new polygon.
    * Implicitly assumes that each next polygon can be attached to an edge of the previous one.
    *
    * This requirement is guaranteed that s is in fact a prefix in the tree traversal (obtained via BFS or DFS).
    *
    */
  def compileSeq(s: Seq[CompositePolygon]) = {
    //println(s)
    val h = s.head
    val t = s.tail
    assert(h.isInstanceOf[BasePolygon])

    t.foldLeft(h)((res, pc) => {
      // this should be the case
      assert(pc.isInstanceOf[Attached])
      val ae = pc.asInstanceOf[Attached]
      assert(res.pol.edges.exists(_ =~= ae.e))
      Attached(res, ae.e, ae.attached)
    })
  }


  def validateList(l : List[CompositePolygon]): Boolean = {
    for (i <- l.indices; if i > 0) {
      val prefix = l.take(i)
      val c = l(i).asInstanceOf[Attached]
      val res = prefix.exists(z => z.edges.exists(_ =~= c.e))
      if (!res) {
        return false
      }
    }
    true
  }

  /**
    * Construct smaller polygons out of different traversals of the dependency tree
    */
  def makeTraversals(pc: CompositePolygon): Stream[CompositePolygon] = {
    val size = pc.size

    // compute all subtrees
    val aTrees = computeAllTrees(pc)

    // DFS-traverse subtrees
    val stacks = aTrees.foldLeft(Set.empty: Set[List[CompositePolygon]]) { case (acc, (bc, tree)) =>
      acc + polygonStack(bc, tree, size)
    }
    // BFS-traverse subtrees
    val queues = aTrees.foldLeft(Set.empty: Set[List[CompositePolygon]]) { case (acc, (bc, tree)) =>
      acc + polygonQueue(bc, tree, size)
    }

    assert(stacks.forall(validateList))
    assert(queues.forall(validateList))

    // Vanilla candidates
    val cands0 = compileToCandidates(pc.toList.reverse)
    // Candidates from DFS-traversing subtrees
    val cands1 = for (s <- stacks; c <- compileToCandidates(s)) yield c
    // Candidates from BFS-traversing subtrees
    val cands2 = for (q <- queues; c <- compileToCandidates(q)) yield c
    (cands0 ++ cands1 ++ cands2).toSeq.distinct.sortBy(_.size).toStream
  }

  def compileToCandidates(s: Seq[CompositePolygon]): Set[CompositePolygon] = {
    // take all prefixes
    val prefixes = for {
      i <- 1 until s.size
      pr = s.take(i)
    } yield compileSeq(pr)
    prefixes.toSet
  }
}
