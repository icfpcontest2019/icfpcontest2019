package lambda.js.render

import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * @author Ilya Sergey
  */
object RenderSolution extends GraderWithGraphicsBase  {

  @JSExportTopLevel("graderWithGraphics")
  def greadeWithGraphics(): Unit ={
    super.main()
  }

  // @JSExportTopLevel("graderWithGraphicsAndBoosters")
  def greadeWithGraphicsAndBoosters(): Unit ={
    super.main(true)
  }

}
