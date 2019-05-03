package lambda.contest.checkers

import lambda.contest.ContestConstants._
import lambda.contest.ErrorMessages._
import lambda.contest.checkers.ContestCheckingUtils.checkTaskWellFormed
import lambda.contest.parsers.ContestTaskParser
import lambda.contest.{Booster, Cell, ContestConstants, ContestException, ContestTask}
import lambda.geometry.integer.IPoint

/**
  * Various conversions
  *
  * @author Ilya Sergey
  */
object TaskCreationUtils {

  /**
    * Read a task description from a string
    */
  def stringToContestTask(s: String): ContestTask = {
    val result = ContestTaskParser(s)
    if (result.isEmpty) throw ContestException(MALFORMED_TASK)
    result.get
  }

  /**
    * Create the task matrix
    *
    * @return The matrix and its X/Y sizes.
    */
  def contestTaskToMatrix(t: ContestTask): (TaskMatrix, Int, Int) = {

    // Check that all is fine with the task
    checkTaskWellFormed(t)

    val ContestTask(room, pos, obstacles, boosters) = t

    val ((xl, yl), (xr, yr)) = room.boundingBox

    // Initial matrix
    val matrix = Array.fill(xr)(Array.fill(yr)(new Cell()))

    for {i <- 0 until xr // right BB boundary not included
         j <- 0 until yr // top BB boundary not included
         cell = IPoint(i, j)} {
      val cellInRoom = room.containsCell(cell)
      val cellNotInObstacles = obstacles.forall(o => !o.containsCell(cell))

      // Add space
      if (cellInRoom && cellNotInObstacles) {
        matrix(i)(j).clearSpace()
      }

    }

    //Put boosters in the room
    for ((b, IPoint(bx, by)) <- boosters) {

      // A bit of paranoia here
      if (!(bx >= 0 && by >= 0 && bx < xr && by < yr))
        throw ContestException(BOOSTER_NOT_IN_ROOM)

      b match {
        case Booster.CallPoint => matrix(bx)(by).setCallPoint()
        case bb => matrix(bx)(by).setBooster(bb)
      }

    }

    (matrix, xr, yr)
  }

  def printContestMatrixInAscii(matrix: TaskMatrix, xsize: Int, ysize: Int, initPos: IPoint): Unit = {
    val wallChar = '#'
    val watchmanChar = 'W'

    for (i <- 0 until xsize + 2) print(wallChar)
    println()
    for (j1 <- 1 to ysize) {
      val j = ysize - j1
      print("#")
      for (i <- 0 until xsize) {
        val c = matrix(i)(j)
        if (c.hasCallPoint) {
          print(CALL_POINT_LETTER)
        } else if (c.hasTeleport) {
          print(TELEPORT_LETTER)
        } else if (c.peekBooster.isDefined) {
          print(Booster.toChar(c.peekBooster.get))
        } else if (i == initPos.x && j == initPos.y) {
          assert(c.canStep)
          print(watchmanChar)
        } else if (c.canStep) {
          print(' ')
        } else {
          print(wallChar)
        }

      }
      print(wallChar)
      println()
    }
    for (i <- 0 until xsize + 2) print(wallChar)
    println()

  }


}
