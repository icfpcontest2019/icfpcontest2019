package lambda.js


import lambda.contest.checkers.TaskCreationUtils.contestTaskToMatrix
import lambda.contest.checkers.{ContestTaskUtils, TaskExecution}
import lambda.contest.{ContestException, ContestTask}
import lambda.geometry.integer.IPoint
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
  lazy val textArea = document.getElementById(outTextFieldId)

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


  def processTaskAndSolution(): Unit = {
    if (!taskFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_TASK_FILE)
      return
    }

    val taskReader = new FileReader()
    taskReader.onloadend = event => {
      val text = taskReader.result.toString
      processSolution(text)
    }
    taskReader.readAsText(taskFileInput.files(0))
  }

  def processSolution(taskText: String): Unit = {
    if (!solutionFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_SOLUTION_FILE)
      return
    }

    val solutionReader = new FileReader()
    solutionReader.onloadend = event => {
      val solText = solutionReader.result.toString
      setText(CHECKING_TEXT)
      val act = () => runSolution(taskText, solText)
      dom.window.setTimeout(act, 50)
    }
    solutionReader.readAsText(solutionFileInput.files(0))
  }



  def setText(text: String): Boolean = {
    textArea.textContent = text
    true
  }


  def runSolution(taskText: String, solutionText: String): Unit = {
    try {
      val task@ContestTask(_, init, _, _) = parseTask(taskText)
      ContestTaskUtils.checkTaskWellFormed(task)
      val moves = parseSolution(solutionText)
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
          // val s3 = s"Pre-processing: $tPre sec, checking: $tMain sec."
          s1 + s2 
        case None => FAILED_TEXT
      }
      setText(resultText)
    } catch {
      case ContestException(msg, data) =>
        val text = if (data.isEmpty) msg else s"$msg ${data.get.toString}"
        val errorText = s"Failed: $text"
        setText(errorText)
    }
  }

}
