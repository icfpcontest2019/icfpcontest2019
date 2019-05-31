package lambda.js

import lambda.contest.ContestConstants.Action
import lambda.contest.checkers.TaskCreationUtils.matrixCopy
import lambda.contest.checkers.TaskExecution.createState
import lambda.contest.checkers.{ContestTaskUtils, TaskCreationUtils, TaskExecution, TaskMatrix}
import lambda.contest.{Booster, ContestException, ContestTask, Watchman}
import lambda.geometry.integer.IPoint
import lambda.geometry.integer.IntersectionUtils.cellsIntersectedByViewSegment
import lambda.js.JSRenderingUtils._
import org.scalajs.dom
import org.scalajs.dom.ext.Color
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
  lazy val speedText = document.getElementById(speedTextId)

  // Make submission buttons
  mkFileInput(centered, submitTaskId, SUBMIT_TASK_TEXT)
  mkFileInput(centered, submitSolutionId, SUBMIT_SOLUTION_TEXT)
  mkButton(centered, execButtonId, EXECUTE_TEXT)

  private val scaleFactorX: Double = 3.8 / 5
  private val scaleFactorY: Double = 4.55 / 5
  private val upperBorder = 28
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
  var currentMatrix: Option[ProcessedTask] = None

  var execIntervalHandle: Option[Int] = None
  val defaultSpeed: Int = 50
  var currentSpeed: Int = defaultSpeed
  var paused: Boolean = true

  /* ------------------------------------------------------------------------ */
  /*                               Main                                       */
  /* ------------------------------------------------------------------------ */

  @JSExportTopLevel("graderWithGraphics")
  def main(): Unit = {
    canvas.width = dims._1
    canvas.height = dims._2 + upperBorder
    clearMain
    setText(UPLOAD_FILES)
    setSpeedText
    refreshExecButton
    taskFileInput.onchange = taskHandler
    solutionFileInput.onchange = solutionHandler
    execButton.onclick = execHandler
    dom.window.onkeypress = { e: dom.KeyboardEvent =>
      blurAllInputs()
      e.keyCode match {
        // Space (s)
        case 32 | 115 => pauseResumeHandler()
        // 'r'
        case 114 => execHandler(e)
        // 'd'
        case 100 => increaseSpeed()
        // 'a'
        case 97 => decreaseSpeed()
        case _ =>
      }
    }
    dom.window.onkeydown = { e: dom.KeyboardEvent =>
      e.keyCode match {
        // Right 
        case 39 => increaseSpeed()
        // Left
        case 37 => decreaseSpeed()
        case _ =>
      }
    }
  }

  /* ------------------------------------------------------------------------ */
  /*                           Controlling execution                          */
  /* ------------------------------------------------------------------------ */

  def pauseResumeHandler(): Unit = {
    paused = !paused
  }

  def setOnPause(): Unit = {
    paused = true
  }

  def increaseSpeed(): Unit = {
    if (currentSpeed >= 4000) return
    if (currentSpeed >= 2000) {
      currentSpeed = 4000
    } else if (currentSpeed >= 1000) {
      currentSpeed = 2000
    } else if (currentSpeed >= 400) {
      currentSpeed = 1000
    } else if (currentSpeed < 25) {
      currentSpeed = currentSpeed + 1
    } else {
      currentSpeed = currentSpeed * 2
    }
    setSpeedText()
    changeExecutionSpeed()
  }

  def decreaseSpeed(): Unit = {
    if (currentSpeed <= 1) return
    if (currentSpeed >= 4000) {
      currentSpeed = 2000
    } else if (currentSpeed >= 2000) {
      currentSpeed = 1000
    } else if (currentSpeed >= 1000) {
      currentSpeed = 400
    } else if (currentSpeed <= 25) {
      currentSpeed = currentSpeed - 1
    } else {
      currentSpeed = currentSpeed / 2
    }
    setSpeedText()
    changeExecutionSpeed()
  }

  def getCurrentRefreshRateMS = {
    1000 / currentSpeed
  }

  def changeExecutionSpeed(): Unit = {
    if (execIntervalHandle.isDefined) {
      dom.window.clearInterval(execIntervalHandle.get)
      val handle = dom.window.setInterval(() => {
        tryRunSolution()
      }, getCurrentRefreshRateMS)
      execIntervalHandle = Some(handle)
    }
  }

  private def interruptExecution = {
    if (execIntervalHandle.isDefined) {
      dom.window.clearInterval(execIntervalHandle.get)
      execIntervalHandle = None
      currentState = None
      setOnPause()
      refreshExecButton
    }
  }


  /* ------------------------------------------------------------------------ */
  /*                         Running solution                                 */
  /* ------------------------------------------------------------------------ */

  def tryRunSolution(): Unit = {
    if (currentState.isDefined) {
      val state = currentState.get
      if (state.getElapsedTime == 0) {
        setText(SPACE_TO_RUN_TEXT)
      } else {
        setText(SPACE_TO_RESUME_TEXT)
      }
    }
    if (!paused) runSolution()
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
            interruptExecution
            val text = if (data.isEmpty) msg else s"$msg ${data.get.toString}"
            val errorText = s"Failed: $text"
            setText(errorText, TEXT_RED)
            refreshExecButton
            currentState = None
          // enableFileInputs
        }
      } else {
        interruptExecution
        if (state.checkFullIllumination()) {
          val steps = state.getElapsedTime
          setText(s"Success! Your solution took $steps time units.", TEXT_GREEN)
        } else {
          setText("Not all parts of the task were covered.", TEXT_RED)
        }
        refreshExecButton
        // enableFileInputs
        currentState = None
      }
    } else {
      // enableFileInputs
      currentState = None
    }
  }

  /* ------------------------------------------------------------------------ */
  /*                                 Drawing                                  */
  /* ------------------------------------------------------------------------ */

  private def callback(painter: JSCanvasPainter)
                      (m: TaskMatrix, dx: Int, dy: Int,
                       watchmen: Map[Int, Watchman],
                       watchPos: Map[Int, IPoint],
                       watchPosOld: Map[Int, IPoint],
                       timeElapsed: Int): Unit = {

    // Get all watchmen at their current positions
    val watchmenPositions: Seq[(Watchman, IPoint)] =
      for {k <- watchmen.keySet.toList
           if watchPos.isDefinedAt(k)
           wPos = watchPos(k)
           w = watchmen(k)} yield (w, wPos)


    val watchmenPositionsOld: Seq[(Watchman, IPoint)] =
      for {k <- watchmen.keySet.toList
           if watchPosOld.isDefinedAt(k)
           wPos = watchPosOld(k)
           w = watchmen(k)} yield (w, wPos)


    // Re-draw illumination in affected cells
    val cells = (for ((w, wPos) <- watchmenPositions ++ watchmenPositionsOld;
                      pc <- getAffectedCells(m, dx, dy, painter.affectedRadius, wPos, w)) yield pc).toSet
    cells.foreach { case (p, c) => painter.renderCell(p, c) }

    // Draw watchmen torch range
    for ((w, wPos) <- watchmenPositions;
         lit <- w.getTorchRange(wPos)) {
      cells.find { case (p, _) => p == lit } match {
        case Some((p, c))
          if c.canStep && c.isIlluminated && squareIsVisible(wPos, p, m, dx, dy) =>
          painter.renderCellWithColor(lit, DARK_YELLOW)
        case _ =>
      }
    }

    // Draw remaining boosters in affected cells
    cells
      .flatMap { case (p, _) => getAffectedNeighbours(p, painter.affectedRadius, m, dx, dy) }
      .foreach { case (p, c) =>
        c.peekBooster match {
          case Some(b) => painter.drawCirclePoint(p, boosterToColor(b))
          case None =>
        }
        if (c.hasCallPoint) {
          painter.drawCirclePoint(p, boosterToColor(Booster.CallPoint))
        } else if (c.hasTeleport) {
          painter.drawCirclePoint(p, INSTALLED_TELE_COLOR)
        }
      }

    // Draw watchmen
    for ((_, wPos) <- watchmenPositions) {
      painter.drawCirclePoint(wPos, RED)
    }

    setText(s"$RUNNING_TEXT: $timeElapsed rounds")
  }

  private def positionWithinBoundingBox(pos: IPoint, dx: Int, dy: Int): Boolean = {
    val (x, y) = pos.toPair
    x >= 0 && y >= 0 && x < dy && y < dy
  }


  private def squareIsVisible(wPos: IPoint, litSquare: IPoint, matrix: TaskMatrix, dx: Int, dy: Int): Boolean = {
    if (!positionWithinBoundingBox(litSquare, dx, dy)) return false
    val crossedCells = cellsIntersectedByViewSegment(wPos, litSquare)
    crossedCells.forall(p => {
      val (x, y) = p.toPair
      matrix(x)(y).canStep
    })
  }


  // Draw the initial task
  private def drawTask(task: ContestTask, painter: JSCanvasPainter) = {
    painter.drawPoly(task.room, LIGHT_GRAY)
    task.obstacles.foreach(o => painter.drawPoly(o, DARK_GRAY))
    for ((b, bp) <- task.boosters) {
      painter.drawCirclePoint(bp, boosterToColor(b))
    }
    painter.drawCirclePoint(task.initPos, RED)
  }

  /* ------------------------------------------------------------------------ */
  /*                                Bookkeeping                               */
  /* ------------------------------------------------------------------------ */

  def setText(text: String, color: Color = TEXT_WHITE): Boolean = {
    clearCaption
    ctx.font = s"${getFontSize(text)}px sans-serif"
    ctx.textAlign = "center"
    ctx.textBaseline = "middle"
    ctx.fillStyle = color.toHex
    ctx.fillText(text, canvas.width / 2, 15)
    ctx.fillStyle = DARK_GRAY.toHex
    true
  }

  def setSpeedText(): Unit = {
    speedText.textContent = s"$currentSpeed"
  }

  private def clearCaption: Unit = {
    ctx.fillStyle = DARK_GRAY.toHex
    ctx.fillRect(0, 0, canvas.width, upperBorder)
  }

  def clearMain: Unit = {
    ctx.fillStyle = DARK_GRAY.toHex
    ctx.fillRect(0, 0, canvas.width, canvas.height - upperBorder)
  }

  private def clearTask = {
    currentTaskText = ""
    currentTask = None
    currentPainter = None
    currentState = None
    currentMatrix = None
  }

  private def clearSolution = {
    currentSolutionText = ""
    currentSolution = None
    currentState = None
  }

  private def disableFileInputs: Unit = {
    taskFileInput.disabled = true
    solutionFileInput.disabled = true
  }

  private def enableFileInputs: Unit = {
    taskFileInput.disabled = false
    solutionFileInput.disabled = false
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

  /* ------------------------------------------------------------------------ */
  /*                               Handlers                                   */
  /* ------------------------------------------------------------------------ */

  def blurAllInputs(): Unit = {
    taskFileInput.blur()
    solutionFileInput.blur()
    execButton.blur()
  }

  val execHandler: Function1[Event, Unit] = event => {
    interruptExecution

    if (currentTask.isDefined && currentSolution.isDefined) {
      setText(PREPROCESSING_TEXT, TEXT_YELLOW)

      val act = () => {
        val task = currentTask.get
        clearMain
        drawTask(task, currentPainter.get)
        val z@(matrix, dx, dy) = currentMatrix match {
          case Some((m, xmax, ymax)) => (matrixCopy(m, xmax, ymax), xmax, ymax)
          case None =>
            val t@(m, xmax, ymax) = TaskCreationUtils.contestTaskToMatrix(task)
            currentMatrix = Some(t)
            (matrixCopy(m, xmax, ymax), xmax, ymax)
        }
        currentState = Some(createState(matrix, dx, dy, task.initPos, currentSolution.get, Nil))
        // disableFileInputs
        val handle = dom.window.setInterval(() => {
          tryRunSolution()
        }, getCurrentRefreshRateMS)
        execIntervalHandle = Some(handle)
      }

      // Oh, this is nasty...
      dom.window.setTimeout(act, 50)
    }
  }

  private val taskHandler: Function1[Event, Unit] = event => {
    if (!taskFileInput.files(0).isInstanceOf[Blob]) {
      clearMain
      setText(NO_TASK_FILE, TEXT_RED)
      clearTask
      refreshExecButton
    } else {
      val taskReader = new FileReader()
      taskReader.onloadend = _ => {
        val text = taskReader.result.toString
        if (text == currentTaskText) {
        } else {
          setText(UPLOADING_TASK, TEXT_YELLOW)
          val act = () => {
            try {
              clearMain
              clearTask
              val task@ContestTask(room, init, _, _) = parseTask(text)
              ContestTaskUtils.checkTaskWellFormed(task)
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
                val text = if (data.isEmpty) msg else s"$msg, ${data.get.toString}"
                val errorText = s"Failed: $text"
                setText(errorText, TEXT_RED)
                refreshExecButton
            }
          }
          dom.window.setTimeout(act, 50)
        }
      }
      taskReader.readAsText(taskFileInput.files(0))
    }
  }

  val solutionHandler: Function1[Event, Unit] = event => {
    interruptExecution

    if (!solutionFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_SOLUTION_FILE, TEXT_RED)
      clearSolution
      refreshExecButton
    } else {
      val solutionReader = new FileReader()
      solutionReader.onloadend = _ => {
        val text = solutionReader.result.toString
        if (text == currentSolutionText) {}
        else {
          setText(UPLOADING_SOLUTION, TEXT_YELLOW)
          val act = () => {
            try {
              clearSolution
              val moves = parseSolution(text)
              currentSolution = Some(moves)
              setText(UPLOADED_SOLUTION)
              refreshExecButton
            } catch {
              case ContestException(msg, data) =>
                clearSolution
                val text = if (data.isEmpty) msg else s"$msg, ${data.get.toString}"
                val errorText = s"Failed: $text"
                setText(errorText, TEXT_RED)
                refreshExecButton
            }
          }
          dom.window.setTimeout(act, 50)
        }
      }
      solutionReader.readAsText(solutionFileInput.files(0))
    }
  }


}