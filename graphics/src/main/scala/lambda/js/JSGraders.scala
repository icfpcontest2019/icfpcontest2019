package lambda.js

import lambda.js.render.GraderWithGraphics
import lambda.js.validate.GraderNoGraphics
import lambda.js.puzzle.PuzzleValidator

import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * @author Ilya Sergey
  */
object JSGraders {
  
  //////////////////////////////////////////////////
  // TODO: Comment me in phase 2
  //////////////////////////////////////////////////


  @JSExportTopLevel("validate")
  def grade(): Unit = GraderNoGraphics.main(false)

  @JSExportTopLevel("render")
  def render(): Unit = GraderWithGraphics.main(false)
  
  //////////////////////////////////////////////////
  // TODO: Uncomment me in phase 2
  //////////////////////////////////////////////////

  
//  @JSExportTopLevel("puzzle")
//  def puzzle(): Unit = PuzzleValidator.main()
//
//  @JSExportTopLevel("validate")
//  def grade(): Unit = GraderNoGraphics.main(true)
//
//  @JSExportTopLevel("render")
//  def render(): Unit = GraderWithGraphics.main(true)
  
  
  
  val _ = PuzzleValidator

}
