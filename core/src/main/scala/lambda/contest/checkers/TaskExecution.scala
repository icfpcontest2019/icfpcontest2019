package lambda.contest.checkers

import lambda.MStack
import lambda.contest.ContestConstants._
import lambda.contest.ContestErrorMessages._
import lambda.contest.{Booster, Cell, ContestConstants, ContestException, Watchman}
import lambda.geometry.integer.IPoint
import lambda.geometry.integer.IntersectionUtils._

import scala.collection.mutable.{Map => MMap, Set => MSet}

/**
  * Global state for validating solutions.
  *
  * @author Ilya Sergey
  */

/**
  * A task execution instance
  *
  */
class TaskExecution(private val matrix: TaskMatrix,
                    val xmax: Int, val ymax: Int,
                    private val routes: Map[Int, MStack[Action]]) {

  private var timeElapsed: Int = 0

  def tick(): Unit = {
    timeElapsed = timeElapsed + 1
  }

  private val watchmen: MMap[Int, Watchman] = MMap.empty
  private val watchmenPositions: MMap[Int, IPoint] = MMap.empty

  // Boosters collected during the execution
  private val availableBoosters: MMap[Booster.Value, Int] = MMap.empty

  /* --------------------------------------------------- */
  /*          Checking runtime properties                */
  /* --------------------------------------------------- */

  private def getCell(point: IPoint): Cell = {
    val (x, y) = point.toPair
    matrix(x)(y)
  }

  private def assertCondition(cond: => Boolean): Unit = {
    if (!cond) throw ContestException(BAD_ACTION)
  }


  private def checkWatchman[T](watchNum: Int, routes: Map[Int, T]): Boolean = {
    routes.isDefinedAt(watchNum) &&
      watchmenPositions.isDefinedAt(watchNum) &&
      watchmen.isDefinedAt(watchNum)
  }

  private def squareIsVisible(wPos: IPoint, litSquare: IPoint): Boolean = {
    if (!positionWithinBoundingBox(litSquare)) return false
    val crossedCells = cellsIntersectedByViewSegment(wPos, litSquare)
    crossedCells.forall(p => getCell(p).canStep)
  }

  private def castLight(w: Watchman, wPos: IPoint): Unit = {
    for {litSquare@IPoint(x, y) <- w.getTorchRange(wPos)
         if squareIsVisible(wPos, litSquare)} {
      val c = matrix(x)(y)
      c.shedLight()
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

  /**
    * Having a drill, can change the walls within the bounding box
    */
  private def moveWithDrill(watchNum: Int, wPosOld: IPoint, act: Action): IPoint = {
    val (x, y) = wPosOld.toPair

    def moveToNewPosWithDrill(wPosNew: IPoint) = {
      assertCondition(positionWithinBoundingBox(wPosNew))
      getCell(wPosNew).clearSpace()
      watchmenPositions(watchNum) = wPosNew
      wPosNew
    }

    act match {
      case MoveUp => moveToNewPosWithDrill(IPoint(x, y + 1))
      case MoveDown => moveToNewPosWithDrill(IPoint(x, y - 1))
      case MoveLeft => moveToNewPosWithDrill(IPoint(x - 1, y))
      case MoveRight => moveToNewPosWithDrill(IPoint(x + 1, y))
      case _ => throw ContestException(BAD_ACTION)
    }
  }

  /**
    * Performs the action, returns new position if moved.
    */
  private def doAct(watchNum: Int, w: Watchman, wPosOld: IPoint, act: Action): IPoint = {

    // Special case for having a drill
    if (act.isMove && w.isDrillGuy) {
      return moveWithDrill(watchNum, wPosOld, act)
    }

    def moveToNewPos(wPosNew: IPoint) = {
      assertCondition(canStepToPosition(wPosNew))
      watchmenPositions(watchNum) = wPosNew
      wPosNew
    }

    val (x, y) = wPosOld.toPair
    act match {
      case MoveUp => moveToNewPos(IPoint(x, y + 1))
      case MoveDown => moveToNewPos(IPoint(x, y - 1))
      case MoveLeft => moveToNewPos(IPoint(x - 1, y))
      case MoveRight => moveToNewPos(IPoint(x + 1, y))

      case TurnLeft => w.rotateTorchLeft(); wPosOld
      case TurnRight => w.rotateTorchRight(); wPosOld

      case Snooze => wPosOld

      case UseBatteries(dx, dy) =>
        // TODO: Implement me!
        wPosOld

      case UseCoffee =>
        // TODO: Implement me!
        wPosOld

      case UseDrill =>
        // TODO: Implement me!
        wPosOld

      case UseTeleport(x, y) =>
        // TODO: Implement me!
        wPosOld

      case UseCallFriend =>
        // TODO: Implement me!
        wPosOld

    }

  }

  /* --------------------------------------------------- */
  /*           Stepping via the routs                    */
  /* --------------------------------------------------- */


  // TODO: Handle the cases when boosters are bought and leased
  // TODO: A single execution step from a given cell

  private def step(watchNum: Int): Unit = {
    if (!checkWatchman(watchNum, routes))
      throw ContestException(WATCHMAN_NOT_FOUND)

    val wPosOld@IPoint(x, y) = watchmenPositions(watchNum)
    val w = watchmen(watchNum)
    val route = routes(watchNum)

    // Step 1: Update illumination at w's position
    castLight(w, wPosOld)

    // Step 2: Collect boosters
    collectBooster(wPosOld)

    // Step 3: Move/Act, removing the current action
    val act = route.pop().getOrElse(Snooze)
    var wPosNew = doAct(watchNum, w, wPosOld, act)

    // Step 3': Repeat the last movement if w is under coffee fumes
    if (act.isMove && w.isUnderCoffe) {
      castLight(w, wPosNew)
      // TODO: Make another step
      // TODO: Implement me!
    }

    // Step 4: Decrement boosters
    w.decrementActiveBoosters()

    // Step 5: Update illumination again
    castLight(w, wPosNew)

  }

  private def evalRound(): Unit = {
    val wNums = watchmen.keys.toList.sorted
    for (wNum <- wNums) {
      step(wNum)
    }
    tick()

    // TODO: Provide a callback (taking the updated room,
    //       the old and the new watchmen positions) for rendering
  }

  /**
    * A driver loop and a final checker
    *
    * @param purchasedBoosters A set of purchased boosters
    */
  def evalSolution(purchasedBoosters: List[(Booster.Value, Int)]): Option[Int] = {
    addPurchasedBoosters(purchasedBoosters)
    while (moreStepsToDo()) {
      evalRound()
    }

    // Final check: everything should be illuminated
    if (checkFullIllumination())
      Some(timeElapsed)
    else
      None
  }

  private def moreStepsToDo(): Boolean = {
    watchmen.keys.exists { wNum =>
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

  private def checkFullIllumination(): Boolean = {
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

  def printState() = {
    val positions = watchmenPositions.values.toList
    TaskCreationUtils.printContestMatrixInAscii(matrix, xmax, ymax, positions)
    println()
  }


}


// Companion object for creating an execution instance
object TaskExecution {

  /**
    *
    * @param matrix    - room matrix
    * @param xmax      - X-boundary
    * @param ymax      - Y-boundary
    * @param initPos   - initial position of a watchman
    * @param routeList - a list of routes
    * @return
    */
  def createState(matrix: TaskMatrix,
                  xmax: Int, ymax: Int,
                  initPos: IPoint,
                  routeList: List[List[Action]],
                  torchShape: TorchShape = DEFAULT_CONTEST_TORCH): TaskExecution = {

    val entries = routeList.zipWithIndex.map { case (r, i) => i -> new MStack(r) }
    val routes = Map(entries: _*)

    val state = new TaskExecution(matrix, xmax, ymax, routes)

    // Create initial watchman
    val initWatchman = new Watchman(MSet(torchShape: _*))
    val firstGuyIndex = 0
    state.watchmen.update(firstGuyIndex, initWatchman)
    state.watchmenPositions.update(firstGuyIndex, initPos)

    // Return the state
    state
  }
}























