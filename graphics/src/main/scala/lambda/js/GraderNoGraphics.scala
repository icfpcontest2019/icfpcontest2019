package lambda.js


import lambda.contest.checkers.TaskCreationUtils.contestTaskToMatrix
import lambda.contest.checkers.TaskExecution
import lambda.contest.{ContestException, ContestTask}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw._

import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExportTopLevel


/**
  * This file allows for client-side grading of contest applications 
  * using Scala.JS compilation
  *
  * @author Ilya Sergey
  */

object GraderNoGraphics extends JSGrading {

  lazy val taskFileInput = document.getElementById(submitTaskId).asInstanceOf[HTMLInputElement]
  lazy val solutionFileInput = document.getElementById(submitSolutionId).asInstanceOf[HTMLInputElement]
  lazy val checkButton = document.getElementById(checkButtonId).asInstanceOf[HTMLButtonElement]
  lazy val textArea = document.getElementById(outTextFieldId).asInstanceOf[HTMLTextAreaElement]

  @JSExportTopLevel("graderNoGraphics")
  def main(canvas: html.Canvas): Unit = {
    val centered = document.getElementById("main_section")

    mkFileInput(centered, submitTaskId, SUBMIT_TASK_TEXT)
    mkFileInput(centered, submitSolutionId, SUBMIT_SOLUTION_TEXT)
    mkButton(centered, checkButtonId, CHECK_TEXT)
    mkTextField(centered, outTextFieldId)
    checkButton.onclick = (e: Event) => {
      setText(LOADING_TEXT)
      processTaskAndSolution()
    }
  }
  
  /* ---------------------------------------------------------------- */
  /*                          Solution checking                       */
  /* ---------------------------------------------------------------- */

  def setText(text: String): Unit = {
    textArea.textContent = text
  }


  def runSolution(taskText: String, solutionText: String): Unit = {
    try {
      val task@ContestTask(_, init, _, _) = parseTask(taskText)
      val moves = parseSolution(solutionText)
      setText(PREPROCESSING_TEXT)
      val t0 = new Date().getTime()
      val (matrix, dx, dy) = contestTaskToMatrix(task)
      val t1 = new Date().getTime()
      val state = TaskExecution.createState(matrix, dx, dy, init, moves, Nil)
      setText(CHECKING_TEXT)
      val finalState = state.evalSolution()
      val resultText = finalState match {
        case Some(steps) =>
          val t2 = new Date().getTime()
          val tPre = (t1 - t0) / 1000
          val tMain = (t2 - t1) / 1000
          val s1 = "Success! \n"
          val s2 = s"Your solution took $steps time units. \n"
          val s3 = s"Pre-processing: $tPre sec, checking: $tMain sec."
          s1 + s2 + s3
        case None => FAILED_TEXT
      }
      setText(resultText)
    } catch {
      case ContestException(msg, data) =>
        val text = if (data.toString.isEmpty) msg else s"$msg (${data.toString})"
        val errorText = s"Failed: $text"
        setText(errorText)
    }
  }

}
