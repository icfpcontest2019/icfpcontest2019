package lambda.contest.checkers

import lambda.MStack
import lambda.contest.ContestConstants._
import lambda.contest.ContestErrorMessages._
import lambda.contest.checkers.TaskExecution.{CallBack, dummyCallback}
import lambda.contest.{ActiveWheelsBooster, ActiveDrillBooster, Booster, Cell, ContestConstants, ContestException, Worker}
import lambda.geometry.integer.IPoint
import lambda.geometry.integer.IntersectionUtils._

import scala.collection.mutable
import scala.collection.mutable.{Map => MMap, Set => MSet}

/**
  * A task execution instance
  *
  * @author Ilya Sergey
  */
class TaskExecution(private val matrix: TaskMatrix,
                    val xmax: Int, val ymax: Int,
                    // Boosters collected during the execution
                    private val availableBoosters: MMap[Booster.Value, Int] = MMap.empty, 
                    private val routes: Map[Int, MStack[Action]]) {

  private var timeElapsed: Int = 0

  def tick(): Unit = {
    timeElapsed = timeElapsed + 1
  }

  private val workers: MMap[Int, Worker] = MMap.empty
  private val workerPositions: MMap[Int, IPoint] = MMap.empty
  private val workerPositionsOld: MMap[Int, IPoint] = MMap.empty

  /* --------------------------------------------------- */
  /*          Checking runtime properties                */
  /* --------------------------------------------------- */

  private def getCell(point: IPoint): Cell = {
    val (x, y) = point.toPair
    matrix(x)(y)
  }

  private def assertCondition(cond: => Boolean, pos: IPoint): Unit = {
    if (!cond) throw ContestException(BAD_ACTION, Some(pos))
  }


  private def checkWorker[T](workNum: Int): Boolean = {
    workerPositions.isDefinedAt(workNum) &&
      workers.isDefinedAt(workNum)
  }

  private def squareIsVisible(wPos: IPoint, litSquare: IPoint): Boolean = {
    if (!positionWithinBoundingBox(litSquare)) return false
    val crossedCells = cellsIntersectedByViewSegment(wPos, litSquare)
    crossedCells.forall(p => getCell(p).canStep)
  }

  private def castLight(w: Worker, wPos: IPoint): Unit = {
    for {litSquare@IPoint(x, y) <- w.getTorchRange(wPos)
         if positionWithinBoundingBox(litSquare)} {
      val cell = getCell(litSquare)
      if (cell.canStep &&
        !cell.isIlluminated &&
        squareIsVisible(wPos, litSquare)) {
        cell.shedLight()
      }
    }
  }

  def positionWithinBoundingBox(pos: IPoint): Boolean = {
    val (x, y) = pos.toPair
    x >= 0 && y >= 0 && x < xmax && y < ymax
  }

  def canStepToPosition(pos: IPoint): Boolean = {
    if (!positionWithinBoundingBox(pos)) return false
    val (x, y) = pos.toPair
    val c = matrix(x)(y)
    c.canStep
  }

  private def collectBooster(wPosOld: IPoint) = {
    val (x, y) = wPosOld.toPair
    val cell = matrix(x)(y)
    cell.collectBooster() match {
      case Some(b) =>
        val bnum = availableBoosters.getOrElse(b, 0)
        availableBoosters.put(b, bnum + 1)
      case None =>
    }
  }

  private def mkNewPosition(wPosOld: IPoint, act: Action): IPoint = {
    val (x, y) = wPosOld.toPair
    act match {
      case MoveUp => IPoint(x, y + 1)
      case MoveDown => IPoint(x, y - 1)
      case MoveLeft => IPoint(x - 1, y)
      case MoveRight => IPoint(x + 1, y)
      case _ => throw ContestException(BAD_ACTION, Some(wPosOld))
    }
  }

  /**
    * Having a drill, can change the walls within the bounding box
    */
  private def moveWithDrill(workNum: Int, wPosOld: IPoint, act: Action): IPoint = {
    def moveToNewPosWithDrill(wPosNew: IPoint) = {
      assertCondition(positionWithinBoundingBox(wPosNew), wPosOld)
      getCell(wPosNew).clearSpace()
      workerPositionsOld(workNum) = wPosOld
      workerPositions(workNum) = wPosNew
      wPosNew
    }

    val wPosNew = mkNewPosition(wPosOld, act)
    act match {
      case MoveUp | MoveDown | MoveLeft | MoveRight =>
        moveToNewPosWithDrill(wPosNew)
      case _ => throw ContestException(BAD_ACTION, Some(wPosOld))
    }
  }


  /**
    * Having a coffee one can make an optional second step
    */
  private def secondStepWithWheels(workNum: Int, wPosOld: IPoint, act: Action): IPoint = {
    val wPosNew = mkNewPosition(wPosOld, act)

    // Move with the drill
    if (workers(workNum).isDrillGuy && positionWithinBoundingBox(wPosNew)) {
      return moveWithDrill(workNum, wPosOld, act)
    }

    def moveToNewPosWithCoffee(wPosOld: IPoint, wPosNew: IPoint) = {
      if (canStepToPosition(wPosNew)) {
        workerPositionsOld(workNum) = wPosOld
        workerPositions(workNum) = wPosNew
        wPosNew
      } else wPosOld
    }

    act match {
      case MoveUp | MoveDown | MoveLeft | MoveRight =>
        moveToNewPosWithCoffee(wPosOld, wPosNew)
      case _ => throw ContestException(BAD_ACTION, Some(wPosOld))
    }
  }


  // Use specific booster by a worker at a given position
  private def useBooster(booster: UseBooster, wNum: Int, wPosOld: IPoint): IPoint = {
    val w = workers(wNum)
    booster match {
      case UseWheels =>
        w.addActiveBooster(new ActiveWheelsBooster)
        wPosOld

      case UseArm(dx, dy) =>
        w.addArm(dx, dy)
        wPosOld

      case UseDrill =>
        w.addActiveBooster(new ActiveDrillBooster)
        wPosOld

      case InstallTele =>
        val cell = getCell(wPosOld)
        try {
          cell.installNewTeleport(wPosOld)
        } catch {
          case ContestException(msg, _) =>
            throw ContestException(msg, Some(wPosOld))
        }
        wPosOld

      case UseCall =>
        val cell = getCell(wPosOld)
        if (cell.hasCallPoint) {
          val newWorker = new Worker()
          val newWNum = workers.keys.max + 1
          workers.update(newWNum, newWorker)
          workerPositionsOld(newWNum) = wPosOld
          workerPositions.update(newWNum, wPosOld)
          wPosOld
        } else {
          throw ContestException(CANNOT_CALL_FRIEND, Some(wPosOld))
        }
    }
  }


  // Use booster if it's available
  private def tryUseBooster(act: Action, wNum: Int, wPosOld: IPoint): IPoint = {
    if (!act.isInstanceOf[UseBooster]) {
      throw ContestException(BAD_BOOSTER, Some(wPosOld))
    }

    val booster: Booster.Value = act match {
      case UseArm(dx, dy) => Booster.ArmBooster
      case UseWheels => Booster.WheelsBooster
      case UseDrill => Booster.DrillBooster
      case InstallTele => Booster.TeleBooster
      case UseCall => Booster.CallBooster
      case _ => throw ContestException(BAD_BOOSTER, Some(wPosOld))
    }

    if (!availableBoosters.isDefinedAt(booster) ||
      availableBoosters(booster) <= 0) {
      throw ContestException(NO_BOOSTER, Some(wPosOld))
    }

    val quantity = availableBoosters(booster)
    if (quantity == 1) {
      availableBoosters.remove(booster)
    } else {
      availableBoosters(booster) = quantity - 1
    }
    useBooster(act.asInstanceOf[UseBooster], wNum, wPosOld)
  }


  def doTeleport(workNum: Int, wPosNew: IPoint, wPosOld: IPoint): IPoint = {
    if (canStepToPosition(wPosNew)) {
      val cell = getCell(wPosNew)
      if (!cell.hasTeleport) {
        throw ContestException(BAD_TELEPORT_LOCATION, Some(wPosNew))
      }
      workerPositionsOld(workNum) = wPosOld
      workerPositions(workNum) = wPosNew
      wPosNew
    } else {
      throw ContestException(BAD_TELEPORT_LOCATION, Some(wPosNew))
    }

  }

  /**
    * Performs the action, returns new position if moved.
    */
  private def doAct(workNum: Int, w: Worker, wPosOld: IPoint, act: Action): IPoint = {

    // Special case for having a drill
    if (act.isMove && w.isDrillGuy) {
      return moveWithDrill(workNum, wPosOld, act)
    }

    def moveToNewPos(wPosNew: IPoint) = {
      assertCondition(canStepToPosition(wPosNew), wPosOld)
      workerPositionsOld(workNum) = wPosOld
      workerPositions(workNum) = wPosNew
      wPosNew
    }

    act match {
      case MoveUp => moveToNewPos(mkNewPosition(wPosOld, act))
      case MoveDown => moveToNewPos(mkNewPosition(wPosOld, act))
      case MoveLeft => moveToNewPos(mkNewPosition(wPosOld, act))
      case MoveRight => moveToNewPos(mkNewPosition(wPosOld, act))

      case TurnLeft => w.rotateTorchLeft(); wPosOld
      case TurnRight => w.rotateTorchRight(); wPosOld

      case Snooze => wPosOld // Do nothing

      case UseArm(_, _) => tryUseBooster(act, workNum, wPosOld)
      case UseWheels => tryUseBooster(act, workNum, wPosOld)
      case UseDrill => tryUseBooster(act, workNum, wPosOld)
      case InstallTele => tryUseBooster(act, workNum, wPosOld)
      case UseCall => tryUseBooster(act, workNum, wPosOld)

      case DoTele(x, y) => doTeleport(workNum, IPoint(x, y), wPosOld)

    }

  }

  /* --------------------------------------------------- */
  /*           Stepping via the routes                   */
  /* --------------------------------------------------- */

  // TODO: Handle the cases when boosters are bought and leased
  // TODO: A single execution step from a given cell

  private def step(workNum: Int): Unit = {
    if (!checkWorker(workNum))
      throw ContestException(WORKER_NOT_FOUND)

    val wPosOld@IPoint(x, y) = workerPositions(workNum)
    val w = workers(workNum)
    val route = routes.getOrElse(workNum, new MStack())

    // Step 1: Update illumination at w's position
    castLight(w, wPosOld)

    // Step 1': Collect boosters
    collectBooster(wPosOld)

    // Step 2: Move/Act, removing the current action
    val act = route.pop().getOrElse(Snooze)
    var wPosNew = doAct(workNum, w, wPosOld, act)

    // Step 2': Repeat the last movement if w is under coffee fumes
    if (act.isMove && w.isWithWheels) {
      castLight(w, wPosNew)
      collectBooster(wPosNew)
      wPosNew = secondStepWithWheels(workNum, wPosNew, act)
    }

    // Step 3: Decrement boosters
    w.decrementActiveBoosters()

    // Step 4: Update illumination again
    castLight(w, wPosNew)

  }

  def evalRound(callback : CallBack = dummyCallback): Unit = {
    runCallBack(callback)
    val wNums = workers.keys.toList.sorted
    for (wNum <- wNums) {
      step(wNum)
    }
    tick()
    runCallBack(callback)
  }

  private def runCallBack(callback: CallBack) = {
    callback(matrix, xmax, ymax,
      workers.toMap,
      workerPositions.toMap,
      workerPositionsOld.toMap,
      getElapsedTime)
  }

  def getElapsedTime = timeElapsed

  /**
    * A driver loop and a final checker
    */
  def evalSolution(): Option[Int] = {
    while (moreStepsToDo()) {
      evalRound()
    }

    // Final check: everything should be illuminated
    if (checkFullIllumination())
      Some(timeElapsed)
    else
      None
  }
  
  /* --------------------------------------------------- */
  /*              Evaluation utilities                   */
  /* --------------------------------------------------- */

  def moreStepsToDo(): Boolean = {
    workers.keys.exists { wNum =>
      val route = routes.getOrElse(wNum, Nil)
      route.nonEmpty
    }
  }

  private def addPurchasedBoosters(purchased: List[(Booster.Value, Int)]) {
    purchased.foreach { case (b, k) =>
      val quantity = availableBoosters.getOrElse(b, 0)
      availableBoosters.put(b, quantity + k)
    }
  }

  def checkFullIllumination(): Boolean = {
    for {x <- 0 until xmax
         y <- 0 until ymax
         cell = matrix(x)(y)
         if cell.canStep} {
      if (!cell.isIlluminated) {
        return false
      }
    }
    true
  }


  /* --------------------------------------------------- */
  /*           Utilities for testing the checker         */
  /* --------------------------------------------------- */


  def toStringBuffer: mutable.StringBuilder = {
    val positions = workerPositions.values.toList
    val buffer = TaskCreationUtils.printContestMatrixInAscii(matrix, xmax, ymax, positions)
    // Add boosters
    if (availableBoosters.nonEmpty) {
      val boosters = availableBoosters.toList.sorted.map { case (k, v) => s"  $k: $v" }
      buffer.append(s"Available boosters:\n${boosters.sorted.mkString("\n")}\n")
    }

    // Workers positions
    val wPoss = for (k <- workerPositions.keys.toList.sorted) yield s"  W$k: ${
      val IPoint(x, y) = workerPositions(k)
      s"($x, $y)"
    }"
    buffer.append(s"Workers positions:\n${wPoss.sorted.mkString("\n")}\n")

    // Boosters per worker
    if (workers.values.exists(w => w.isDrillGuy || w.isWithWheels)) {
      val ws = workers.toList.map { case (i, w) =>
        val activeBoosters = w.getActiveBoostersWithTime.sorted.map { case (k, v) => s"($k : $v)" }
        s"  W$i: " + activeBoosters.sorted.mkString(" ")
      }
      buffer.append(s"Active boosters:\n${ws.sorted.mkString("\n")}\n")
    }
    
    // Elapsed time
    buffer.append(s"Elapsed time: $timeElapsed")
    
    buffer
  }

  def getWorkerPosition(wNum: Int): Option[IPoint] = {
    workerPositions.get(wNum)
  }

  def isLit(x: Int, y: Int): Boolean = {
    val p = IPoint(x, y)
    if (!positionWithinBoundingBox(p)) return false
    matrix(x)(y).isIlluminated
  }


}


// Companion object for creating an execution instance
object TaskExecution {
  
  type CallBack = (TaskMatrix, Int, Int, 
    Map[Int, Worker], Map[Int, IPoint], Map[Int, IPoint], Int) => Unit
  
  def dummyCallback(m: TaskMatrix, dx: Int, dy: Int,
                    ws: Map[Int, Worker],
                    wpos: Map[Int, IPoint],
                    wposOld: Map[Int, IPoint],
                    timeElapsed: Int) {}

  /**
    *
    * @param matrix    - room matrix
    * @param xmax      - X-boundary
    * @param ymax      - Y-boundary
    * @param initPos   - initial position of a worker
    * @param routeList - a list of routes
    * @return
    */
  def createState(matrix: TaskMatrix,
                  xmax: Int, ymax: Int,
                  initPos: IPoint,
                  routeList: List[List[Action]],
                  initBoosters: List[Booster.Value] = Nil,
                  torchShape: TorchShape = DEFAULT_CONTEST_TORCH): TaskExecution = {

    val entries = routeList.zipWithIndex.map { case (r, i) => i -> new MStack(r) }
    val routes = Map(entries: _*)
    
    val boostersWithCount = initBoosters.distinct.map(b => (b, initBoosters.count(_ == b)))
    
    val state = new TaskExecution(matrix, xmax, ymax, mutable.Map(boostersWithCount: _*), routes)

    // Create initial worker
    val initWorker = new Worker(MSet(torchShape: _*))
    val firstGuyIndex = 0
    state.workers.update(firstGuyIndex, initWorker)
    state.workerPositions.update(firstGuyIndex, initPos)

    // Return the state
    state
  }
}























