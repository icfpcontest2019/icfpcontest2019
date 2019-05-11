package lambda.misc.rendering.old

import lambda.geometry.floating.visibility.VisibilityChecker
import lambda.geometry.floating.{FPoint, FPolygon}
import lambda.misc.artgallery.{CheckInputParser, CheckSolutionParser}

import scala.io.Source

/**
  * @author Ilya Sergey
  */

object ModelCheckSolution {

  def main(args: Array[String]) {
    val path = args(0)
    val map = Source.fromFile(path).getLines.toSeq.map(CheckInputParser(_).get).toMap
    val spath = args(1)
    val sol = Source.fromFile(spath).getLines.toSeq.map(CheckSolutionParser(_).get)

    checkSolution(map, sol)

//    generateAndCheckSolution(map)
    //testExhausively(map)
  }

  def generateAndCheckSolution(map: Map[Int, (FPolygon, Seq[FPoint])]): Unit = {
    for (k <- map.keySet.toSeq.sorted) {
      val (pol, guards) = map(k)
      val res = VisibilityChecker.checkVisibility(pol, guards)
      assert(!res._1)
      val (ce, msg) = res._2.get
      print(s"$k: $ce")
      println()
    }
  }

  def checkSolution(map: Map[Int, (FPolygon, Seq[FPoint])], sol: Seq[(Int, FPoint)]): Unit = {
    for ((k, ce) <- sol) {
      val (pol, guards) = map(k)
      val (res, reason) = VisibilityChecker.isInvisible(pol, guards, ce)
      if (!res) {
        println(reason)
      }
      assert(res)
      println(s"$k: tested for $ce, okay ")
    }

  }
}
