package lambda.geometry.integer

import lambda.geometry.integer.examples.WatchmanExamples._
import org.scalatest.{FlatSpec, Matchers}

class IPolygonTests extends FlatSpec with Matchers {

  /**
    * Testing that all defailt polygons are valid
    */
  for (i <- roomPolygons.indices) {
    val room = roomPolygons(i)
    s"A watchman room number ${i + 1}" should "pass the well-formedness check" in {
      // Basic validity
      room.isWellFormed

      // TODO: Test the is on the left
    }

    it should "be rectilinear" in room.isRectilinear

  }


}
