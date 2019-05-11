package lambda.geometry.floating.generators.maps

import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.generators.PolygonGeneratorInstance
import org.scalacheck.Arbitrary

class MapGeneratorInstance(polygons: PolygonGeneratorInstance, minObstacles: Int, maxObstacles: Int) {
  def generate() = MapPropertyUtils.mapGenerator(polygons, minObstacles, maxObstacles)

  implicit lazy val arbMapCombinator: Arbitrary[Seq[FPolygon]] = Arbitrary(generate())
}