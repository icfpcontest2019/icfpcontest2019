package lambda.js.validate

import lambda.contest.checkers.TaskCreationUtils.matrixCopy
import lambda.contest.checkers.TaskExecution.createState
import lambda.contest.checkers.{ContestTaskUtils, TaskCreationUtils, TaskExecution}
import lambda.contest.{Booster, ContestException, ContestTask}
import lambda.js.JSGrading
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw._


/**
  * This file allows for client-side grading of contest applications 
  * using Scala.JS compilation
  *
  * @author Ilya Sergey
  */

object GraderNoGraphics extends JSGrading {
  
  def main(createBoosters: Boolean = false): Unit = {
    val centered = document.getElementById("main_section")

    mkFileInput(centered, submitTaskId, SUBMIT_TASK_TEXT)
    mkFileInput(centered, submitSolutionId, SUBMIT_SOLUTION_TEXT)
    if (createBoosters) {
      mkFileInput(centered, submitBoostersId, SUBMIT_BOOSTERS_TEXT)
      boostersFileInput.onchange = boosterHandler
    }
    mkButton(centered, execButtonId, CHECK_TEXT)
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

  private def clearBoosters = {
    currentBoosterText = ""
    currentBoosters = Nil
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
              currentSolutionText = text
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
        val state = createState(matrix, dx, dy, task.initPos, currentSolution.get, currentBoosters)
        runSolution(state)
      }

      // Oh, this is nasty...
      dom.window.setTimeout(act, 50)
    } else {
      setText(CANNOT_PROCESS_TEXT)
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

  protected val boosterHandler: Function1[Event, Unit] = event => {
    if (boostersFileInput == null ||
      !boostersFileInput.files(0).isInstanceOf[Blob]) {
      clearBoosters
    }
    else {
      val boosterReader = new FileReader()
      boosterReader.onloadend = _ => {
        val text = boosterReader.result.toString
        if (text == currentBoosterText) {}
        else {
          val act = () => {
            try {
              clearBoosters
              currentBoosters = parseBoosters(text)
              currentBoosterText = text
            } catch {
              case ContestException(msg, data) =>
                clearBoosters
                val text = if (data.isEmpty) msg else s"$msg, ${data.get.toString}"
                val errorText = s"Failed: $text"
                setText(errorText)
            }
          }
          dom.window.setTimeout(act, 50)
        }
      }
      boosterReader.readAsText(boostersFileInput.files(0))
    }
  }


  def setText(text: String): Boolean = {
    textArea.textContent = text
    true
  }

}
