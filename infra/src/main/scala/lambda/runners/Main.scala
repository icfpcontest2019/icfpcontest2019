package lambda.runners

import lambda.geometry.floating.{FPoint, FSegment}
import lambda.geometry.integer.IntersectionUtils._
import lambda.geometry.integer.{IPoint, IntersectionUtils}

/**
  * @author Ilya Sergey
  */
object Main {

  val s5 = FSegment(FPoint(0.5, 0.5), FPoint(2.5, 4.5))

  def main(args: Array[String]): Unit = {
    println("Hello, lambda!")

    cellsForSegment(s5) // List((0,0), (1,1), (1,2), (1,3), (2,3), (2,4))


  }

  def cellsForSegment(s: FSegment): Unit = {
    println(s"For segment ${s}: ${cellsCrossedBySegment(s).map(_.toPair)}") // List((1,1), (2,2))
    println(segmentIntersectsCell(s5, IPoint(1, 4)))
  }
}
