package lambda.contest.generators

import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.generators.PolygonGenerators._
import lambda.geometry.floating.generators.PolygonPropertyUtils._
import lambda.geometry.floating.generators._
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
case class ContestPolygonGenerator(basePolygons: List[FPolygon],
                                   baseFreqs: List[Int],
                                   attachments: List[(LazyPolygon, AttachmentStrategy)],
                                   attFreqs: List[Int],
                                   additionalCond: PolygonCondition,
                                   polySizes: (Int, Int))
  extends PolygonGeneratorInstance {
  
  def cond(p: FPolygon) = {
    val ip = p.toIPolygon
    ip.isWellFormed && ip.isRectilinear
  } 

  def generate(numGenerations: Int): Gen[CompositePolygon] = {
    val finalCond = (p: FPolygon) => cond(p) && additionalCond(p)
    
    for {
      // choose base polygon
      base <- polyFreqs(baseFreqs, basePolygons)
      // attachments
      toAttach = polyFreqs(attFreqs, attachments)
      // edge size
      eSize = Gen.choose(polySizes._1, polySizes._2)
      // number of extensions (generations)
      // gSize <- Gen.choose(1, generations)
    } yield generatePolygon(base, toAttach, finalCond, eSize, numGenerations)

  }
  protected override val numGen = 100
  
}
