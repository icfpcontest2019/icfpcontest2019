package lambda.js

import lambda.js.render.GraderWithGraphics
import lambda.js.validate.GraderNoGraphics

import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * @author Ilya Sergey
  */
object JSGraders {

  //////////////////////////////////////////////////
  // TODO: Comment me in phase 2
  //////////////////////////////////////////////////


  @JSExportTopLevel("validate")
  def grade(): Unit = {
    GraderNoGraphics.main(false)
  }

  @JSExportTopLevel("render")
  def render(): Unit = {
    GraderWithGraphics.main(false)
  }
  
  //////////////////////////////////////////////////
  // TODO: Uncomment me in phase 2
  //////////////////////////////////////////////////
  
/*
  @JSExportTopLevel("validate")
  def grade(): Unit ={
    GraderNoGraphics.main(true)
  }

  @JSExportTopLevel("render")
  def render(): Unit = {
    GraderWithGraphics.main(true)
  }
*/



}
