package lambda.geometry.generators

import lambda.geometry.Polygon
import org.scalacheck.Arbitrary

class MapGeneratorInstance(polygons: PolygonGeneratorInstance, minObstacles: Int, maxObstacles: Int) {
  def generate() = MapPropertyUtils.mapGenerator(polygons, minObstacles, maxObstacles)

  implicit lazy val arbMapCombinator: Arbitrary[Seq[Polygon]] = Arbitrary(generate())
}