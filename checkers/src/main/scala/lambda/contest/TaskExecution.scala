package lambda.contest

import lambda.contest.ContestConstants.Move
import lambda.geometry.integer.IPoint

import scala.collection.mutable.{Map => MMap, Set => MSet}

/**
  * Global state for validating solutions.
  *
  * @author Ilya Sergey
  */

// Companion object for creating an execution instane
object TaskExecution {

  def createState(room: Array[Array[Cell]],
                  xmax: Int, ymax: Int,
                  initPos: IPoint,
                  routes: List[List[Move]]) : TaskExecution = {

    val routeMap: MMap[Int, List[Move]] = MMap.empty
    for (i <- routes.indices) {
      routeMap.put(i + 1, routes(i))
    }


    val state = new TaskExecution(room, xmax, ymax, routeMap)

    // Create initial watchman
    val initWatchman = new Watchman()
    state.watchmen.update(1, initWatchman)
    state.watchmenPositions.update(1, initPos)
    state.activeWatchmen = 1

    // Return the state
    state
  }
}


/**
  * A task execution instance
  */
class TaskExecution(private val room: Array[Array[Cell]],
                    val xmax: Int, val ymax: Int,
                    private val routes: MMap[Int, List[Move]]) {

  private var time: Int = 0

  private var activeWatchmen: Int = 0
  private val watchmen: MMap[Int, Watchman] = MMap.empty
  private val watchmenPositions: MMap[Int, IPoint] = MMap.empty

  // Boosters collected during the execution
  private val availableBoosters: MSet[Booster.Value] = MSet.empty
  // Necessary for buying boosters
  private val boostersToBuy: MSet[Booster.Value] = MSet.empty


  // TODO: Handle the cases when boosters are bought and leased
  // TODO: A single execution step from a given cell

  private def step(wNum: Int, canLeaseBoosters: Boolean): Unit = {

  }

  // TODO: An execution round
  private def round(canLeaseBoosters: Boolean): Unit = {
    // TODO:


    time = time + 1
  }


  // TODO: A driver loop and a final checker
  /**
    *
    * @param canLeaseBoosters false when no leasing is allowed
    */
  def eval(canLeaseBoosters: Boolean = false): (Int, List[(Booster.Value, Int)]) = {
    // TODO: while for active watchmen there is stuff to do, do it


    // Which boosters were leased and for how much time
    val leasedBoosters = if (canLeaseBoosters) {
      val grouped = boostersToBuy.toList.groupBy(b => b)
      grouped.toList.map{case (b, l) => (b, l.size)}
    } else Nil

    (time, leasedBoosters)
  }


}























