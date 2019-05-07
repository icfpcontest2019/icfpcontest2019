package lambda.geometry.floating.generators.maps

import lambda.geometry.floating.generators.PolygonGeneratorInstance
import lambda.geometry.floating.{FPoint, FPolygon}
import org.scalacheck.Gen


object MapPropertyUtils {
  // Increase to allow objects to move further away earlier in placement.
  val LAX_SLOPE = 0.1

  def mapGenerator(obstacleGenerator: PolygonGeneratorInstance, minObstacles: Int, maxObstacles: Int): Gen[Seq[FPolygon]] = {
    var r = scala.util.Random

    for {
      obstacleCount <- Gen.choose(minObstacles, maxObstacles)
      obstacles <- Gen.listOfN(obstacleCount, obstacleGenerator.generateFlat(100))
    } yield {
      var placed = List[FPolygon]()

      for (obstacle <- obstacles) yield {
        var moved = obstacle
        var lax = 1
        do {
          // Allow more moves as we grow tired of waiting
          val neworigin = new FPoint(
            r.nextGaussian()*lax*LAX_SLOPE,
            r.nextGaussian()*lax*LAX_SLOPE
          )
          moved = moved.rotateCWAndShift(r.nextGaussian(), neworigin)
          lax += 1
        } while (placed.exists(_ intersect moved))

        placed = moved :: placed

        moved
      }
    }
  }
}
