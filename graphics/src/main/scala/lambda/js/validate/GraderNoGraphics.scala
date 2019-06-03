package lambda.js.validate

import lambda.contest.checkers.TaskCreationUtils.matrixCopy
import lambda.contest.checkers.TaskExecution.createState
import lambda.contest.checkers.{ContestTaskUtils, TaskCreationUtils, TaskExecution}
import lambda.contest.{ContestException, ContestTask}
import lambda.js.JSGrading
import org.scalajs.dom
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

  lazy val taskFileInput = document.getElementById(submitTaskId).asInstanceOf[HTMLInputElement]
  lazy val solutionFileInput = document.getElementById(submitSolutionId).asInstanceOf[HTMLInputElement]
  lazy val execButton = document.getElementById(checkButtonId).asInstanceOf[HTMLButtonElement]
  lazy val textArea = document.getElementById(outTextFieldId)

  var currentTaskText: String = ""
  var currentSolutionText: String = ""
  var currentTask: Option[ContestTask] = None

  var currentSolution: Option[TaskSolution] = None
  var currentState: Option[TaskExecution] = None
  var currentMatrix: Option[ProcessedTask] = None


  @JSExportTopLevel("graderNoGraphics")
  def main(canvas: html.Canvas): Unit = {
    val centered = document.getElementById("main_section")

    mkFileInput(centered, submitTaskId, SUBMIT_TASK_TEXT)
    mkFileInput(centered, submitSolutionId, SUBMIT_SOLUTION_TEXT)
    mkButton(centered, checkButtonId, CHECK_TEXT)
    mkTextField(centered, outTextFieldId)
    taskFileInput.onchange = taskHandler
    solutionFileInput.onchange = solutionHandler
    execButton.onclick = execHandler
  }

  private def clearTask = {
    currentTaskText = ""
    currentTask = None
    currentState = None
    currentMatrix = None
  }

  private def clearSolution = {
    currentSolutionText = ""
    currentSolution = None
    currentState = None
  }


  /* ---------------------------------------------------------------- */
  /*                          Solution checking                       */
  /* ---------------------------------------------------------------- */


  private val taskHandler: Function1[Event, Unit] = event => {
    if (!taskFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_TASK_FILE)
      clearTask
    } else {
      val taskReader = new FileReader()
      taskReader.onloadend = _ => {
        val text = taskReader.result.toString
        if (text == currentTaskText) {
        } else {
          setText(UPLOADING_TASK)
          val act = () => {
            try {
              clearTask
              val task@ContestTask(room, init, _, _) = parseTask(text)
              ContestTaskUtils.checkTaskWellFormed(task)
              currentTaskText = text
              currentTask = Some(task)
              setText(UPLOADED_TASK)
            } catch {
              case ContestException(msg, data) =>
                clearTask
                val text = if (data.isEmpty) msg else s"$msg, ${data.get.toString}"
                val errorText = s"Failed: $text"
                setText(errorText)
            }
          }
          dom.window.setTimeout(act, 50)
        }
      }
      taskReader.readAsText(taskFileInput.files(0))
    }
  }

  private val solutionHandler: Function1[Event, Unit] = event => {
    if (!solutionFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_SOLUTION_FILE)
      clearSolution
    } else {
      val solutionReader = new FileReader()
      solutionReader.onloadend = _ => {
        val text = solutionReader.result.toString
        if (text == currentSolutionText) {}
        else {
          setText(UPLOADING_SOLUTION)
          val act = () => {
            try {
              clearSolution
              val moves = parseSolution(text)
              currentSolution = Some(moves)
              setText(UPLOADED_SOLUTION)
            } catch {
              case ContestException(msg, data) =>
                clearSolution
                val text = if (data.isEmpty) msg else s"$msg, ${data.get.toString}"
                val errorText = s"Failed: $text"
                setText(errorText)
            }
          }
          dom.window.setTimeout(act, 50)
        }
      }
      solutionReader.readAsText(solutionFileInput.files(0))
    }
  }

  val execHandler: Function1[Event, Unit] = event => {
    if (currentTask.isDefined && currentSolution.isDefined) {
      setText(PREPROCESSING_TEXT)

      val act = () => {
        val task = currentTask.get
        val z@(matrix, dx, dy) = currentMatrix match {
          case Some((m, xmax, ymax)) => (matrixCopy(m, xmax, ymax), xmax, ymax)
          case None =>
            val t@(m, xmax, ymax) = TaskCreationUtils.contestTaskToMatrix(task)
            currentMatrix = Some(t)
            (matrixCopy(m, xmax, ymax), xmax, ymax)
        }
        val state = createState(matrix, dx, dy, task.initPos, currentSolution.get, Nil)
        runSolution(state)
      }

      // Oh, this is nasty...
      dom.window.setTimeout(act, 50)
    }
  }

  
  private def runSolution(state: TaskExecution): Unit = {
    try {
      setText(CHECKING_TEXT)
      val finalState = state.evalSolution()
      val resultText = finalState match {
        case Some(steps) =>
          val s1 = "Success! \n"
          val s2 = s"Your solution took $steps time units. \n"
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


  def setText(text: String): Boolean = {
    textArea.textContent = text
    true
  }

}
