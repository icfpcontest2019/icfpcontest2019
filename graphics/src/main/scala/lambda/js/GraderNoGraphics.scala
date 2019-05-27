package lambda.js


import org.scalajs.dom._
import org.scalajs.dom.raw._

import scala.scalajs.js.annotation.JSExportTopLevel


/**
  * This file allows for client-side grading of contest applications 
  * using Scala.JS compilation
  *
  * @author Ilya Sergey
  */

object GraderNoGraphics extends JSGrading {

  val outTextField = "output"
  val checkButton = "check_solution"
  val submitTask = "submit_task"
  val submitSolution = "submit_solution"

  lazy val textArea = document.getElementById(outTextField).asInstanceOf[HTMLTextAreaElement]
  lazy val taskFileInput = document.getElementById(submitTask).asInstanceOf[HTMLInputElement]
  lazy val solutionFileInput = document.getElementById(submitSolution).asInstanceOf[HTMLInputElement]

  @JSExportTopLevel("graderNoGraphics")
  def main(canvas: html.Canvas): Unit = {
    val centered = document.createElement("center")
    document.body.appendChild(centered)
    // What to do for checking solutions
    val checkHandler = (e: Event) => {
      setText(LOADING_TEXT)
      processTaskAndSolution()
    }

    mkFileInput(centered, submitTask, SUBMIT_TASK_TEXT)
    mkFileInput(centered, submitSolution, SUBMIT_SOLUTION_TEXT)
    mkButton(centered, checkButton, SUBMIT_TEXT, checkHandler)
    mkTextField(centered, outTextField)
  }
  
  def setText(text: String): Unit = {
    textArea.textContent = text
  }

}
