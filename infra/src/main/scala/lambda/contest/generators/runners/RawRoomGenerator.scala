package lambda.contest.generators.runners

import lambda.contest.generators.geodata.GeoHelper
import lambda.contest.generators.{ContestGenerators, GeneratorRendering}
import lambda.geometry.floating.{FPolygon, FPolygonUtils}
import lambda.geometry.floating.generators.CompositePolygon

import scala.util.Random

/**
  * Generating rectilinear polygons
  *
  * @author Ilya Sergey
  */

object RawRoomGenerator extends GeneratorRendering {

  var needLollis : Boolean = false


  def main(args: Array[String]) {
    if (args.length < 1) {
      println("Bad format, please specify the size of the bounding box!")
      return 
    }
    
    val boxSize = args(0).toInt
    needLollis = if (args.length >= 2) {
      args(1).toBoolean
    } else false
    
    val boundOpt = if (args.length >= 3) {
      Some(GeoHelper.getCountryScaled(args(2), boxSize))
    } else None
    
    
    draw(boxSize, boundOpt)
  
  }

  
  protected def generateNewPolygon(boxSize: Int = 100, boundOpt: Option[FPolygon]): CompositePolygon = {
    
    val numGen = 50 + Random.nextInt(150)
    val generator = ContestGenerators.roomGenerator(boxSize, needLollis, boundOpt)
    val pc = generator.generate(numGen).sample.get
    
    val ipol = pc.pol.toIPolygon.shiftToOrigin
    assert(ipol.isWellFormed && ipol.isRectilinear)
    
    println()
    val (x, y) = ipol.dimensions
    println(s"Bounding box: $x x $y")
    println(s"${ipol.vertices.size} vertices")

    val area = FPolygonUtils.computeArea(ipol.toFPolygon)
    val ratio = area / (boxSize * boxSize) * 100
    
    println(s"Area covered: ${ratio.toInt}%")
    println(ipol.vertices.mkString(", "))
    
    println()
    pc
  }

}




