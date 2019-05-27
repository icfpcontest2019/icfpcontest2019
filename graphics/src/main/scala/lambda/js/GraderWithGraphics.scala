package lambda.js

import lambda.contest.ContestConstants.Action
import lambda.contest.checkers.{TaskCreationUtils, TaskExecution, TaskMatrix}
import lambda.contest.{Booster, ContestException, ContestTask}
import lambda.js.JSColors._
import org.scalajs.dom
import org.scalajs.dom.raw.{FileReader, _}
import org.scalajs.dom.{html, _}

import scala.scalajs.js.annotation.JSExportTopLevel


/**
  * @author Ilya Sergey
  */
object GraderWithGraphics extends JSGrading {

  lazy val canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]
  lazy val ctx: CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  lazy val centered = document.getElementById("main_section")
  lazy val taskFileInput = document.getElementById(submitTaskId).asInstanceOf[HTMLInputElement]
  lazy val solutionFileInput = document.getElementById(submitSolutionId).asInstanceOf[HTMLInputElement]
  lazy val checkButton = document.getElementById(checkButtonId).asInstanceOf[HTMLButtonElement]
  // Make submission buttons
  mkFileInput(centered, submitTaskId, SUBMIT_TASK_TEXT)
  mkFileInput(centered, submitSolutionId, SUBMIT_SOLUTION_TEXT)
  mkButton(centered, checkButtonId, CHECK_AND_RENDER_TEXT)


  private val scaleFactor: Double = 4.5 / 5

  private val upperBorder = 30

  lazy val dim: Int = {
    val myWidth = dom.window.innerWidth.toInt
    val myHeight = dom.window.innerHeight.toInt
    val dim = (math.min(myWidth, myHeight).toDouble * scaleFactor).toInt
    dim
  }

  private def getFontSize(s: String): Int = {
    // (getDimension.toDouble / s.length * 1.5).toInt
    15
  }


  /* ------------------------------------------------------------------------ */
  /*                         Mutable stuff                                    */
  /* ------------------------------------------------------------------------ */
  
  type ProcessedTask = (TaskMatrix, Int, Int)
  type TaskSolution = List[List[Action]]
  
  var currentTaskText : String = ""
  var currentSolutionText : String = ""
  var currentPainter : Option[JSCanvasPainter] = None
  var currentTask : Option[(ContestTask, ProcessedTask)] = None
  var currentSolution : Option[TaskSolution] = None

  private def uploadTaskAndSolution(): Unit = {
    if (!taskFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_TASK_FILE)
      return
    }
    val taskReader = new FileReader()
    taskReader.onloadend = event => {
      val text = taskReader.result.toString
      if (text == currentTaskText) {
        uploadSolution()
      } else {
        try {
          val task@ContestTask(room, init, _, _) = parseTask(text)
          val painter = new JSCanvasPainter(ctx, room, dim, dim, upperBorder)
          val z@(matrix, dx, dy) = TaskCreationUtils.contestTaskToMatrix(task)
          currentTaskText = text
          currentTask = Some(task, z)
          currentPainter = Some(painter)
          setText(UPLOADED_TASK)
          drawTask(task, painter)
          uploadSolution()
        } catch {
          case ContestException(msg, data) =>
            currentTaskText = ""
            currentTask = None
            currentPainter = None
            val text = if (data.toString.isEmpty) msg else s"$msg (${data.toString})"
            val errorText = s"Failed: $text"
            setText(errorText)
        }
      }
    }
    taskReader.readAsText(taskFileInput.files(0))
  }

  private def uploadSolution(): Unit = {
    if (!solutionFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_SOLUTION_FILE)
      return
    }

    val solutionReader = new FileReader()
    solutionReader.onloadend = event => {
      val text = solutionReader.result.toString
      if (text == currentSolutionText) {}
      else {
        try {
          val moves = parseSolution(text)
          currentSolution = Some(moves)
          setText(UPLOADED_ALL)
        } catch {
          case ContestException(msg, data) =>
            currentSolutionText = ""
            currentSolution = None
            val text = if (data.toString.isEmpty) msg else s"$msg (${data.toString})"
            val errorText = s"Failed: $text"
            setText(errorText)
        }
      }
      
    }
    solutionReader.readAsText(solutionFileInput.files(0))
  }

  private def drawTask(task: ContestTask, painter: JSCanvasPainter) = {
    painter.drawPoly(task.room, LIGHT_GRAY)
    task.obstacles.foreach(o => painter.drawPoly(o, DARK_GRAY, stroke = true))
    for ((b, bp) <- task.boosters) {
      painter.drawCirclePoint(bp, boosterToColor(b))
    }
    painter.drawCirclePoint(task.initPos, RED)

  }

  /* ------------------------------------------------------------------------ */
  /*                         Running solution                                 */
  /* ------------------------------------------------------------------------ */


  def runSolution(taskText: String, solutionText: String): Unit = {
    try {
      val task@ContestTask(room, init, _, _) = parseTask(taskText)
      val painter = new JSCanvasPainter(ctx, room, dim, dim, upperBorder)
      drawTask(task, painter)
      val moves = parseSolution(solutionText)
      val (matrix, dx, dy) = TaskCreationUtils.contestTaskToMatrix(task)
      var state = TaskExecution.createState(matrix, dx, dy, init, moves, Nil)
      while (state.moreStepsToDo()) {
        state.evalRound()
      }
      val resultText = if (state.checkFullIllumination()) {
        val steps = state.getElapsedTime
        val s1 = "Success! \n"
        val s2 = s"Your solution took $steps time units."
        s1 + s2
      } else FAILED_TEXT
      setText(resultText)
    } catch {
      case ContestException(msg, data) =>
        val text = if (data.toString.isEmpty) msg else s"$msg (${data.toString})"
        val errorText = s"Failed: $text"
        setText(errorText)
    }
  }




  /* ------------------------------------------------------------------------ */
  /*                         Main rendering                                   */
  /* ------------------------------------------------------------------------ */

  def setText(text: String): Unit = {
    clearCaption
    ctx.font = s"${getFontSize(text)}px sans-serif"
    ctx.textAlign = "center"
    ctx.textBaseline = "middle"
    ctx.fillStyle = "white"
    ctx.fillText(text, canvas.width / 2, 15)
    ctx.fillStyle = DARK_GRAY.toHex
  }

  private def clearCaption: Unit = {
    ctx.fillStyle = DARK_GRAY.toHex
    ctx.fillRect(0, 0, canvas.width, upperBorder)
  }

  def clearMain: Unit = {
    ctx.fillStyle = DARK_GRAY.toHex
    ctx.fillRect(0, 0, canvas.width, canvas.height - upperBorder)
  }
  
  @JSExportTopLevel("graderWithGraphics")
  def main(): Unit = {
    canvas.height = dim + upperBorder
    canvas.width = dim
    clearMain
    setText(UPLOAD_FILES)
    checkButton.onclick = (e: Event) => {
      clearMain
      setText(PREPROCESSING_TEXT)
      uploadTaskAndSolution()
    }
  }


}