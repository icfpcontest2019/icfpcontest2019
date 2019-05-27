package lambda.js

import lambda.contest.ContestConstants.Action
import lambda.contest.checkers.{TaskCreationUtils, TaskExecution, TaskMatrix}
import lambda.contest.{ContestException, ContestTask, Watchman}
import lambda.geometry.integer.IPoint
import lambda.js.JSColors._
import org.scalajs.dom
import org.scalajs.dom.raw.{FileReader, _}
import org.scalajs.dom.{html, _}

import scala.scalajs.js.annotation.JSExportTopLevel


/**
  * @author Ilya Sergey
  */
object GraderWithGraphics extends JSGrading {

  /* ------------------------------------------------------------------------ */
  /*                         Boring rendering                                 */
  /* ------------------------------------------------------------------------ */

  lazy val canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]
  lazy val ctx: CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  lazy val centered = document.getElementById("main_section")
  lazy val taskFileInput = document.getElementById(submitTaskId).asInstanceOf[HTMLInputElement]
  lazy val solutionFileInput = document.getElementById(submitSolutionId).asInstanceOf[HTMLInputElement]
  lazy val execButton = document.getElementById(execButtonId).asInstanceOf[HTMLButtonElement]

  // Make submission buttons
  mkFileInput(centered, submitTaskId, SUBMIT_TASK_TEXT)
  mkFileInput(centered, submitSolutionId, SUBMIT_SOLUTION_TEXT)
  mkButton(centered, execButtonId, EXECUTE_TEXT)

  private val scaleFactorX: Double = 4.0 / 5
  private val scaleFactorY: Double = 4.6 / 5
  private val upperBorder = 30
  lazy val dims: (Int, Int) = {
    val myWidth = dom.window.innerWidth.toInt
    val myHeight = dom.window.innerHeight.toInt
    ((myWidth.toDouble * scaleFactorX).toInt, (myHeight.toDouble * scaleFactorY).toInt)
  }

  private def getFontSize(s: String): Int = {
    // (getDimension.toDouble / s.length * 1.5).toInt
    15
  }

  /* ------------------------------------------------------------------------ */
  /*                  Mutable stuff and its manipulation                      */
  /* ------------------------------------------------------------------------ */

  type ProcessedTask = (TaskMatrix, Int, Int)
  type TaskSolution = List[List[Action]]

  var currentTaskText: String = ""
  var currentSolutionText: String = ""
  var currentPainter: Option[JSCanvasPainter] = None
  var currentTask: Option[ContestTask] = None
  var currentSolution: Option[TaskSolution] = None
  var currentState: Option[TaskExecution] = None
  var execIntervalHandle: Option[Int] = None

  private def drawTask(task: ContestTask, painter: JSCanvasPainter) = {
    painter.drawPoly(task.room, LIGHT_GRAY)
    task.obstacles.foreach(o => painter.drawPoly(o, DARK_GRAY))
    for ((b, bp) <- task.boosters) {
      painter.drawCirclePoint(bp, boosterToColor(b))
    }
    painter.drawCirclePoint(task.initPos, RED)
    // setText(s"K = ${painter.scalingCoefficient}")
  }

  /* ------------------------------------------------------------------------ */
  /*                         Running solution                                 */
  /* ------------------------------------------------------------------------ */

  private def callback(painter: JSCanvasPainter)
                      (m: TaskMatrix, dx: Int, dy: Int,
                       watchmen: Map[Int, Watchman],
                       watchPos: Map[Int, IPoint]): Unit = {
    // TODO: This is supeer-slow, draw it more efficiently using only the changing bits around watchmen! 
    //    for {
    //      i <- 0 until dx
    //      j <- 0 until dx
    //      c = m(i)(j)
    //      sq = IPoint(i, j).toSquare
    //    } {
    //      painter.drawPoly(sq, if (c.canStep) LIGHT_GRAY else DARK_GRAY)
    //    }

    for (w <- watchmen.keySet.toList; wp = watchPos(w)) {
      painter.drawCirclePoint(wp, RED)
    }
  }

  def runSolution(): Unit = {
    if (allInPlace) {
      val painter = currentPainter.get
      val state = currentState.get
      if (state.moreStepsToDo()) {
        try {
          state.evalRound(callback(painter))
        } catch {
          case ContestException(msg, data) =>
            stopExecution
            val text = if (data.toString.isEmpty) msg else s"$msg (${data.toString})"
            val errorText = s"Failed: $text"
            setText(errorText)
            refreshExecButton
        }
      } else {
        stopExecution
        if (state.checkFullIllumination()) {
          val steps = state.getElapsedTime
          setText(s"Success! Your solution took $steps time units. \n")
        } else {
          setText("Not all parts of the task were covered.")
        }
        refreshExecButton
      }
    }
  }

  /* ------------------------------------------------------------------------ */
  /*                                Rendering                                 */
  /* ------------------------------------------------------------------------ */

  private def stopExecution = {
    if (execIntervalHandle.isDefined) {
      dom.window.clearInterval(execIntervalHandle.get)
      execIntervalHandle = None
    }
  }

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

  /* ------------------------------------------------------------------------ */
  /*                               Handlers                                   */
  /* ------------------------------------------------------------------------ */


  private val taskHandler: Function1[Event, Unit] = event => {
    if (!taskFileInput.files(0).isInstanceOf[Blob]) {
      clearMain
      setText(NO_TASK_FILE)
      clearTask
      refreshExecButton
    } else {
      val taskReader = new FileReader()
      taskReader.onloadend = _ => {
        val text = taskReader.result.toString
        if (text == currentTaskText) {
        } else {
          try {
            clearMain
            setText(UPLOADING_TASK)
            val task@ContestTask(room, init, _, _) = parseTask(text)
            val painter = new JSCanvasPainter(ctx, room, dims._1, dims._2, upperBorder)
            currentTaskText = text
            currentTask = Some(task)
            currentPainter = Some(painter)
            setText(UPLOADED_TASK)
            drawTask(task, painter)
            refreshExecButton
          } catch {
            case ContestException(msg, data) =>
              clearTask
              val text = if (data.toString.isEmpty) msg else s"$msg (${data.toString})"
              val errorText = s"Failed: $text"
              setText(errorText)
              refreshExecButton
          }
        }
      }
      taskReader.readAsText(taskFileInput.files(0))
    }
  }

  val solutionHandler: Function1[Event, Unit] = event => {
    if (!solutionFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_SOLUTION_FILE)
      refreshExecButton
    } else {
      val solutionReader = new FileReader()
      solutionReader.onloadend = _ => {
        val text = solutionReader.result.toString
        if (text == currentSolutionText) {}
        else {
          try {
            setText(UPLOADING_SOLUTION)
            val moves = parseSolution(text)
            currentSolution = Some(moves)
            setText(UPLOADED_SOLUTION)
            refreshExecButton
          } catch {
            case ContestException(msg, data) =>
              clearSolution
              val text = if (data.toString.isEmpty) msg else s"$msg, ${data.toString}"
              val errorText = s"Failed: $text"
              setText(errorText)
              refreshExecButton
          }
        }
      }
      solutionReader.readAsText(solutionFileInput.files(0))
    }
  }

  val execHandler: Function1[Event, Unit] = event => {
    if (currentTask.isDefined && currentSolution.isDefined) {
      val task = currentTask.get
      // TODO: Use asyncs/futures to inform about this processing
      val z@(matrix, dx, dy) = TaskCreationUtils.contestTaskToMatrix(task)
      currentState = Some(TaskExecution.createState(matrix, dx, dy, task.initPos, currentSolution.get, Nil))
      val handle = dom.window.setInterval(() => {
        runSolution()
      }, 20)
      execIntervalHandle = Some(handle)
      execButton.disabled = true
    }
  }

  private def clearTask = {
    currentTaskText = ""
    currentTask = None
    currentPainter = None
    currentState = None
  }

  private def clearSolution = {
    currentSolutionText = ""
    currentSolution = None
    currentState = None
  }

  private def refreshExecButton: Unit = {
    if (allInPlace) {
      execButton.disabled = false
    } else {
      execButton.disabled = true
    }
  }

  // May start execution
  private def allInPlace = {
    currentTask.isDefined &&
      currentSolution.isDefined &&
      currentPainter.isDefined
  }

  @JSExportTopLevel("graderWithGraphics")
  def main(): Unit = {
    canvas.width = dims._1
    canvas.height = dims._2 + upperBorder
    clearMain
    setText(UPLOAD_FILES)
    refreshExecButton
    taskFileInput.onchange = taskHandler
    solutionFileInput.onchange = solutionHandler
    execButton.onclick = execHandler
  }


}