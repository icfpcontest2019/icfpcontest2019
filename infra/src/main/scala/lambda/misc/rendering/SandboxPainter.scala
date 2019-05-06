package lambda.misc.rendering

import lambda.geometry.floating.generators.PolygonCombinatorUtils._
import lambda.geometry.floating.generators.rectilinear.ContestRectilinearPolygonGenerator

/**
  * Generating rectilinear polygons
  *
  * @author Ilya Sergey
  */

object RectoPolyGeneratorToolbox extends PolygonPainter {

  def main(args: Array[String]) {
    
    val generator = ContestRectilinearPolygonGenerator
    val pc = generator.generate().sample.get
    println(pc.pol.vertices.size)
    drawShape(pc)
    println(pc)

  }

}




