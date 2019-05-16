package lambda.contest.checkers

import lambda.contest.ContestConstants._
import lambda.contest.ContestErrorMessages._
import lambda.contest.checkers.ContestTaskUtils._
import lambda.contest.parsers.ContestTaskParser
import lambda.contest.{Booster, Cell, ContestException, ContestTask}
import lambda.geometry.integer.IPoint

import scala.collection.mutable

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
    if (result.isEmpty) throw ContestException(s"$MALFORMED_TASK : ${result.toString}")
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
    val matrix = Array.fill(xr)(Array.fill(yr)(Cell()))

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

  val SOLID_WALL_CHAR = '#'
  val NOTHING_CHAR = '.'
  val NEWLINE = '\n'

  def taskToMatrixString(t: ContestTask) = {
    val buffer = new mutable.Queue[String]()
    val (matrix, dx, dy) = contestTaskToMatrix(t)
    // Add initial position
    buffer.enqueue(t.initPos.toString)
    for {j <- 0 until dy} {
      val line = new mutable.StringBuilder()
      for (i <- 0 until dx; cell = matrix(i)(j)) {
        if (!cell.canStep) {
          line.append(SOLID_WALL_CHAR)
        } else if (cell.hasCallPoint) {
          line.append(CALL_POINT_LETTER)
        } else if (cell.peekBooster.isDefined) {
          line.append(Booster.toChar(cell.peekBooster.get))
        } else {
          line.append(NOTHING_CHAR)
        }
      }
      buffer.enqueue(line.toString())
    }
    buffer.toList.mkString(NEWLINE.toString)
  }

  def stringsToTaskMatrix(ls: List[String]): (TaskMatrix, Int, Int, IPoint) = {
    if (ls.isEmpty) throw ContestException(BAD_TASK_MATRIX)
    import lambda.contest.parsers.ContestTaskParser._

    val initRes = parseAll(intPoint, ls.head)
    if (initRes.isEmpty) throw ContestException(BAD_TASK_MATRIX)
    val initPos = initRes.get
    val rest = ls.tail
    if (rest.isEmpty) throw ContestException(BAD_TASK_MATRIX)
    val dy = rest.size
    val dx = rest.head.length
    val matrix = Array.fill(dx)(Array.fill(dy)(Cell()))
    for (j <- rest.indices) {
      val line = rest(j)
      for (i <- line.indices) {
        val cell = matrix(i)(j)
        line.charAt(i) match {
          case `SOLID_WALL_CHAR` =>  
          case NOTHING_CHAR => cell.clearSpace()
          case CALL_POINT_LETTER => 
            cell.clearSpace()
            cell.setCallPoint()
          case COFFEE_LETTER =>
            cell.clearSpace()
            cell.setBooster(Booster.CoffeeBooster)
          case BATTERIES_LETTER =>
            cell.clearSpace()
            cell.setBooster(Booster.BatteriesBooster)
          case DRILL_LETTER =>
            cell.clearSpace()
            cell.setBooster(Booster.DrillBooster)
          case INTSTALL_TELEPORT_LETTER =>
            cell.clearSpace()
            cell.setBooster(Booster.TeleportBooster)
          case CALL_FRIEND_LETTER =>
            cell.clearSpace()
            cell.setBooster(Booster.CallWatchmanBooster)
          case _ => throw ContestException(BAD_TASK_MATRIX)
        } 
      }
    }
    (matrix, dx, dy, initPos)
  }


  def printContestMatrixInAscii(matrix: TaskMatrix, xsize: Int, ysize: Int, positions: List[IPoint]): StringBuilder = {
    val wallChar = '#'
    val watchmanChar = 'W'
    val buffer = new StringBuilder

    def println(): Unit = {
      buffer.append("\n")
    }

    def print(s: Char): Unit = {
      buffer.append(s)
    }

    for (i <- 0 until xsize + 2) print(wallChar)
    println()
    for (j1 <- 1 to ysize) {
      val j = ysize - j1
      print(wallChar)
      for (i <- 0 until xsize) {
        val c = matrix(i)(j)
        if (positions.contains(IPoint(i, j))) {
          assert(c.canStep)
          print(watchmanChar)
        } else if (c.hasCallPoint) {
          print(CALL_POINT_LETTER)
        } else if (c.hasTeleport) {
          print(INTSTALL_TELEPORT_LETTER)
        } else if (c.peekBooster.isDefined) {
          print(Booster.toChar(c.peekBooster.get))
        } else if (c.isIlluminated) {
          print('.')
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
    buffer

  }


}
