package lambda.contest.checkers

import lambda.MStack
import lambda.contest.ContestConstants.Move
import lambda.contest.{Booster, Cell, Watchman}
import lambda.geometry.integer.IPoint

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
class TaskExecution(private val room: Array[Array[Cell]],
                    val xmax: Int, val ymax: Int,
                    private val routes: Map[Int, MStack[Move]]) {

  private var time: Int = 0

  private var activeWatchmen: Int = 0
  private val watchmen: MMap[Int, Watchman] = MMap.empty
  private val watchmenPositions: MMap[Int, IPoint] = MMap.empty

  // Boosters collected during the execution
  private val availableBoosters: MSet[Booster.Value] = MSet.empty


  // TODO: Handle the cases when boosters are bought and leased
  // TODO: A single execution step from a given cell

  private def step(wNum: Int): Unit = {
    /*
    1. Update illumination
    2. Move/act
    3. Update boosters
    4. Update illumination
     */

  }


  private def round(canLeaseBoosters: Boolean): Unit = {
    // TODO: An execution round for all active watchmen


    // TODO: Provide a callback (taking the updated room,
    //  the old and the new watchmen positions) for rendering
    time = time + 1
  }


  // TODO: A driver loop and a final checker
  /**
    *
    * @param purchasedBoosters A set of purchased boosters
    */
  def eval(purchasedBoosters: List[(Booster.Value, Int)]): Int = {
    // TODO: while for active watchmen there is stuff to do, do it


    0
  }


}


// Companion object for creating an execution instance
object TaskExecution {

  /**
    *
    * @param matrix - room matrix
    * @param xmax - X-boundary
    * @param ymax - Y-boundary
    * @param initPos - initial position of a watchman
    * @param routes - a list of routes
    * @return
    */
  def createState(matrix: TaskMatrix,
                  xmax: Int, ymax: Int,
                  initPos: IPoint,
                  routes: List[List[Move]]) : TaskExecution = {

    val is = routes.indices.toList.map(_ + 1)
    val entries = is.zip(routes).map { case (i, r) => i -> new MStack(r) }
    val routeMap = Map(entries: _*)

    val state = new TaskExecution(matrix, xmax, ymax, routeMap)

    // Create initial watchman
    val initWatchman = new Watchman()
    state.watchmen.update(1, initWatchman) //
    state.watchmenPositions.update(1, initPos)
    state.activeWatchmen = 1

    // Return the state
    state
  }
}






















