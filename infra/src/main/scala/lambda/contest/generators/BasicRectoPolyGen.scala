package lambda.contest.generators

import lambda.geometry.floating.FPolygonUtils
import lambda.geometry.floating.generators.CompositePolygon

import scala.util.Random

/**
  * Generating rectilinear polygons
  *
  * @author Ilya Sergey
  */

object BasicRectoPolyGen extends GeneratorRendering {

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
    
    
    draw(boxSize)
  
  }

  
  protected def generateNewPolygon(boxSize: Int = 100): CompositePolygon = {
    
    val numGen = 100 + Random.nextInt(150)
    val generator = ContestGenerators.roomGenerator(boxSize, needLollis)
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




