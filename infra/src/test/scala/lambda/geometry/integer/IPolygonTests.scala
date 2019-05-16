package lambda.geometry.integer

import lambda.contest.checkers.ContestTaskUtils
import lambda.geometry.integer.examples.WatchmanExamples._
import org.scalatest.{FlatSpec, Matchers}

class IPolygonTests extends FlatSpec with Matchers {

  /**
    * Testing that all defailt polygons are valid
    */
  for (i <- roomsAndPositions.indices) {
    val (room, pos) = roomsAndPositions(i)
    val testName = s"A watchman room number ${i + 1}"
    testRoomAndPosition(room, pos, testName)
  }


  protected def testRoomAndPosition(room: IPolygon, pos: IPoint, testName: String): Unit = {
    testName should "pass the well-formedness check" in {
      // Basic validity
      assert(room.isWellFormed)
    }

    it should "be rectilinear" in {
      assert(room.isRectilinear)
    }

    it should "contains the proposed positions" in {
      assert(room.containsCell(pos))
    }

    it should "be within its bounding box" in {
      ContestTaskUtils.roomWithinPositiveBoundingBox(room)
    }

  }
}
