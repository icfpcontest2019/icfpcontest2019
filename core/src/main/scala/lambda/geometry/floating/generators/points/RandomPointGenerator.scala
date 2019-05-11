package lambda.geometry.floating.generators.points

import lambda.geometry.floating.{FPoint, FPolygon}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by gilles on 06/12/16.
  */
class RandomPointGenerator(nump: Int,
                           r: Random = new Random) {

  def generate(minX: Double, maxX: Double, minY: Double, maxY: Double): Seq[FPoint] = {
    val scaleX = maxX - minX
    val scaleY = maxY - minY

    for (i <- 0 until nump) yield FPoint(scaleX * r.nextDouble + minX, scaleY * r.nextDouble + minY)
  }

  def generatePotentialPoint(scaleX: Double, scaleY: Double, baseX: Double, baseY: Double): FPoint = {
    val p = FPoint(scaleX * r.nextDouble + baseX, scaleY * r.nextDouble + baseY)
    p
  }

  // This method was added since generateOutsidePolygons gets into infinite loops. Definitely needs refactoring
  def generateExternalPoint(scaleX: Double, scaleY: Double, baseX: Double, baseY: Double, polygonsInPlace: Seq[FPolygon]): FPoint = {

    while (true) {
      val potentialPoint = generatePotentialPoint(scaleX, scaleY, baseX, baseY)

      var validFlag = true
      for (polygon <- polygonsInPlace) {
        if (polygon.containsPoint(potentialPoint)) {
          validFlag = false
        }
      }

      if (validFlag) {
        return potentialPoint
      }
    }

    new FPoint(baseX, baseY)
  }


  def generateOutsidePolygons(topLeft: FPoint, bottomRight: FPoint, polygons: Seq[FPolygon]): Seq[FPoint] = {

    var i = 0
    var filteredRandomPoints = ArrayBuffer[FPoint]()

    val scaleX = (bottomRight.x - topLeft.x)
    val scaleY = (bottomRight.y - topLeft.y)

    while (i < nump) {
      val p = generatePotentialPoint(scaleX, scaleY, topLeft.x, topLeft.y)
      var valid = true
      for (poly <- polygons) {
        if (poly.containsPoint(p)) {
          valid = false
        }
      }
      if (valid) {
        filteredRandomPoints += p
        i = i + 1
      }
//      println(p.x +"  "+p.y+"  "+ valid)
    }

    //println(filteredRandomPoints.length)
    filteredRandomPoints
  }


  def randomPointsToString(randomPts: Seq[FPoint]): String = {
    val ptsString = randomPts.map(_.toString).mkString(",")
    //val strs = ptsString.mkString(System.lineSeparator())
    //strs

    ptsString

    //ProjectUtils.writeToNewFile(filePath, pathsString)
  }


}
