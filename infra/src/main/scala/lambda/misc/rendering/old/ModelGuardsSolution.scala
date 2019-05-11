package lambda.misc.rendering.old

import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.triangles.DualGraphUtils
import lambda.geometry.floating.visibility.VisibilityChecker
import lambda.misc.artgallery.{GuardInputParser, GuardSolutionParser}

import scala.io.Source

/**
  * Produce model solution
  */
object ModelGuardsSolution {

  def main(args: Array[String]) {
    val path = args(0)
    val spath = args(1)
    val map = Source.fromFile(path).getLines.toSeq.map(GuardInputParser(_).get).toMap
    val sol = Source.fromFile(spath).getLines.toSeq.map(GuardSolutionParser(_).get)

    for ((pn, guards) <- sol) {
      val pol = map(pn)
      val res = VisibilityChecker.checkVisibility(pol, guards)._1
      assert(res)
      println(s"Tested model solution successfully for polygon ${pn}.")
    }

    //testExhausively(map)
  }

  def generateAndCheckSolution(map: Map[Int, FPolygon]): Unit = {
    for (k <- map.keySet.toSeq.sorted) {
      val pol: FPolygon = map(k)
      val guards = DualGraphUtils.chavatalVisibilitySet(pol)
      val sz = guards.size
      val pvs = pol.vertices.size
      val res = VisibilityChecker.checkVisibility(pol, guards)._1
      assert(res)
      assert(!(sz * 3 > pvs + 1))
      print(s"$k: [${sz}/${pvs}, ${!(sz * 3 > pvs + 1)}], visible: $res ")
      print(guards.mkString(", "))
      println()
    }
  }

  def testExhausively(map: Map[Int, FPolygon]): Unit = {
    for (k <- map.keySet.toSeq.sorted) {
      val pol: FPolygon = map(k)
      val guards = pol.vertices
      val res = VisibilityChecker.checkVisibility(pol, guards)._1
      assert(res)
      println(s"$k: tested, okay ")
    }

  }
}
