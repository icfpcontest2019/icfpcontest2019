package lambda.solvers

import java.io.File

import lambda.contest.ContestConstants.Action
import lambda.contest.checkers.TaskCreationUtils.{contestTaskToMatrix, taskToMatrixString}
import lambda.contest.checkers.TaskMatrix
import lambda.contest.parsers.ContestTaskParser
import lambda.contest.{Cell, Watchman}
import lambda.geometry.integer.IPoint
import lambda.geometry.integer.IntersectionUtils.cellsIntersectedByViewSegment
import lambda.util.FileUtil

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object SimpleSolver {

  def main(args: Array[String]): Unit = {
    val path = args(0)
    val line = FileUtil.readFromFileWithNewLines(new File(args(0)).getAbsolutePath).trim
    val task = ContestTaskParser(line).get
    val (matrix, dx, dy) = contestTaskToMatrix(task)
    val solution = solveTask(matrix, dx, dy, task.initPos)
    println(solution)
  }
  

  def solveTask(matrix: TaskMatrix, xmax: Int, ymax: Int, initPos: IPoint) = {
    val problem = ContestProblem(matrix, xmax, ymax, initPos)
    // Initialize
    problem.doStuff()
    while (problem.hasMoreDarkNeighbours) {
      problem.step()
    }
    problem.getFinalPath
  }


}


case class ContestProblem(matrix: TaskMatrix, xmax: Int, ymax: Int, private var currentPos: IPoint) {


  val watchman = new Watchman()

  val pathUnderConstruction: mutable.Queue[IPoint] = new mutable.Queue[IPoint]()
  val darkNeighbours: collection.mutable.HashSet[IPoint] = new mutable.HashSet[IPoint]()

  def positionWithinBoundingBox(pos: IPoint): Boolean = {
    val (x, y) = pos.toPair
    x >= 0 && y >= 0 && x < xmax && y < ymax
  }

  private def canStepToPosition(pos: IPoint): Boolean = {
    if (!positionWithinBoundingBox(pos)) return false
    val (x, y) = pos.toPair
    val c = matrix(x)(y)
    c.canStep
  }


  private def squareIsVisible(wPos: IPoint, litSquare: IPoint): Boolean = {
    if (!positionWithinBoundingBox(litSquare)) return false
    val crossedCells = cellsIntersectedByViewSegment(wPos, litSquare)
    crossedCells.forall(p => getCell(p).canStep)
  }

  private def getLightableNeighbourCells(wPos: IPoint): List[IPoint] = {
    val squares = for {litSquare <- watchman.getTorchRange(wPos)
                       if positionWithinBoundingBox(litSquare)
                       if canStepToPosition(wPos)
                       if squareIsVisible(wPos, litSquare)
    } yield litSquare
    wPos :: squares
  }


  private def getCell(point: IPoint): Cell = {
    val (x, y) = point.toPair
    matrix(x)(y)
  }

  private def getNeighbours(p: IPoint): List[IPoint] = {
    val IPoint(x, y) = p
    val candidates = List((x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1),
      (x - 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)).map { case (a, b) => IPoint(a, b) }
    candidates.filter(c => positionWithinBoundingBox(c) && canStepToPosition(c))
  }

  private def refreshDarkNeighbours(): Unit = {
    darkNeighbours.filterNot(p => getCell(p).isIlluminated)

    val litCells = getLightableNeighbourCells(currentPos).toSet
    for (c <- litCells;
         n <- getLightableNeighbourCells(c);
         cell = getCell(c)
         if !cell.isIlluminated) {
      darkNeighbours.add(n)
    }
  }

  private def castLight(wPos: IPoint): Unit = {
    for {litSquare <- getLightableNeighbourCells(wPos)} {
      val cell = getCell(litSquare)
      if (cell.canStep &&
        !cell.isIlluminated &&
        squareIsVisible(wPos, litSquare)) {
        cell.shedLight()
      }
    }
    refreshDarkNeighbours()
  }

  // Initializer
  def doStuff() {
    // Add initial position
    pathUnderConstruction.enqueue(currentPos)
    castLight(currentPos)
  }

  def hasMoreDarkNeighbours: Boolean = darkNeighbours.nonEmpty

  // Directions and preference
  private def getNextMoves(p: IPoint): List[(IPoint, Int)] = {
    val IPoint(x, y) = p
    List((IPoint(x + 1, y), 1),
      (IPoint(x, y + 1), 2),
      (IPoint(x - 1, y), 3),
      (IPoint(x, y - 1), 4))
      .filter(p => canStepToPosition(p._1))
  }

  // count the number of new squares lit by moving to point p
  private def countNewLitSquares(p: IPoint) = {
    val neighbours = getLightableNeighbourCells(p)
    neighbours.count(e => !getCell(e).isIlluminated)
  }

  private def lightNewSquare(p: IPoint): Boolean = countNewLitSquares(p) > 0

  //  compare two moves by the number of potential newly lighted squares in reverse order
  private def compareMoveNewLit(p1: IPoint, p2: IPoint) = {
    val new1 = countNewLitSquares(p1)
    val new2 = countNewLitSquares(p2)
    if (new1 < new2) 1 else if (new1 > new2) -1 else 0
  }


  def step(): Unit = {

    if (darkNeighbours.isEmpty) {
      println("No more dark cells to visit.")
      // Nothing else to do
      return
    }

    // Decide on the next best move
    val nextMove: IPoint = getNextMoves(currentPos)
      .groupBy { case (move, pref) => countNewLitSquares(move) }
      .maxBy(_._1)
      ._2
      .minBy(_._2)
      ._1

    // The next move will light some new cells
    if (getLightableNeighbourCells(nextMove).exists(p => !getCell(p).isIlluminated)) {
      currentPos = nextMove
      doStuff()
      // The step is made
      return
    }
    
    // We're stuck, need find a path to a new dark location
    val newPath = searchNewPath(currentPos)
    for (p <- newPath) {
      currentPos = p
      doStuff()
    }
  }
  
  // Searching new dark path 
  def searchNewPath(start: IPoint): List[IPoint] = {
    val queue = new mutable.Queue[IPoint]()
    val parentMap = new mutable.HashMap[IPoint, IPoint]()
    val marked = new mutable.HashSet[IPoint]()

    queue.enqueue(start)
    marked.add(start)

    // Do while didn't find one of the dark neighbours
    while (queue.nonEmpty && !marked.exists(p => darkNeighbours.contains(p))) {

      val pos = queue.dequeue()
      for (n <- getNeighbours(pos)
           if !marked.contains(n)) {
        parentMap.put(n, pos)
        queue.enqueue(n)
        marked.add(n)
      }
    }

    val possibleDestinations = marked.intersect(darkNeighbours)
    val closest = possibleDestinations.minBy(p => math.abs(p.x - start.x) + math.abs(p.y - start.y))

    var path: List[IPoint] = List(closest)
    var tmp = closest
    while (parentMap(tmp) != start) {
      val pred = parentMap(tmp)
      path = pred :: path
      tmp = pred
    }

    path
  }


  private def executeSearchPath(path: List[IPoint]): Unit = {

  }

  def getFinalPath: List[List[Action]] = 
    SolverUtils.convertPointsToMoves(pathUnderConstruction.toList)


  // TODO: track path

  // TODO: track current position


}
