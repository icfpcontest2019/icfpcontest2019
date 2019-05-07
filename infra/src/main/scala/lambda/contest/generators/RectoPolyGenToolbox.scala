package lambda.contest.generators

import lambda.geometry.floating.{FPoint, FPolygonUtils}
import lambda.geometry.floating.generators.CompositePolygon
import lambda.geometry.integer.{IPoint, IPolygon}

import scala.util.Random

/**
  * Generating rectilinear polygons
  *
  * @author Ilya Sergey
  */

object RectoPolyGenToolbox extends GeneratorRendering {

  def main(args: Array[String]) {
    draw(100)
  
  }

  
  protected def generateNewPolygon(boxSize: Int = 100): CompositePolygon = {
    
    val numGen = 150 + Random.nextInt(150)
    val generator = ContestGenerators.simpleRGenerator(boxSize)
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




