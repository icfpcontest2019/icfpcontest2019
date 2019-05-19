package lambda.solvers

import lambda.contest.{Cell, Watchman}
import lambda.contest.checkers.TaskMatrix
import lambda.geometry.integer.IPoint
import lambda.geometry.integer.IntersectionUtils.cellsIntersectedByViewSegment

/**
  * @author Ilya Sergey
  */
object SimpleSolver {
  
  def getNextMoves(problem: ContestProblem, p: IPoint) = {
    val IPoint(x, y) = p
    List(IPoint(x + 1, y), IPoint(x, y + 1), IPoint(x - 1, y), IPoint(x, y - 1))
      .filter(problem.canStepToPosition)
  }
  
  // count the number of new squares lit by moving to point p
  def countNewLitSquares(room: ContestProblem, p: IPoint) = {
    val neighbours = room.getLightableNeighbourCells(p)
    neighbours.count(e => !room.getCell(e).isIlluminated)
  }
  

  def lightNewSquare(room: ContestProblem, p: IPoint): Boolean =
    countNewLitSquares(room, p) > 0
  
  
  //  compare two moves by the number of potential newly lighted squares in reverse order
  def compareMoveNewLit(room: ContestProblem, p1: IPoint, p2: IPoint) = {
    val new1 = countNewLitSquares(room, p1)
    val new2 = countNewLitSquares(room, p2)
    if (new1 < new2) 1 else if (new1 > new2) -1 else 0
  }
  
  // TODO: Comparing move options: build preferences between moves

}

case class ContestProblem(matrix: TaskMatrix, xmax: Int, ymax: Int, private var initPos: IPoint) {
  
  val watchman = new Watchman()
  
  // TODO: track path
  
  // TODO: track current position
  
  def getLightableNeighbourCells(wPos: IPoint): List[IPoint] = {
    val squares = for {litSquare@IPoint(x, y) <- watchman.getTorchRange(wPos)
         if positionWithinBoundingBox(litSquare)} yield litSquare
    wPos :: squares
  }
  
  def getAllAvailableCells: List[Cell] = {
    (for (i <- 0 until xmax; j <- 0 until ymax;
         p = IPoint(i, j) if canStepToPosition(p)) yield getCell(p)).toList
  }

  
  def canStepToPosition(pos: IPoint): Boolean = {
    if (!positionWithinBoundingBox(pos)) return false
    val (x, y) = pos.toPair
    val c = matrix(x)(y)
    c.canStep
  }

  def getCell(point: IPoint): Cell = {
    val (x, y) = point.toPair
    matrix(x)(y)
  }

  private def squareIsVisible(wPos: IPoint, litSquare: IPoint): Boolean = {
    if (!positionWithinBoundingBox(litSquare)) return false
    val crossedCells = cellsIntersectedByViewSegment(wPos, litSquare)
    crossedCells.forall(p => getCell(p).canStep)
  }

  def positionWithinBoundingBox(pos: IPoint): Boolean = {
    val (x, y) = pos.toPair
    x >= 0 && y >= 0 && x < xmax && y < ymax
  }

  private def getNeighbours(p: IPoint): List[IPoint] = {
    val IPoint(x, y) = p
    val candidates = List((x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1),
      (x - 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)).map { case (a, b) => IPoint(a, b) }
    candidates.filter(c => positionWithinBoundingBox(c) && canStepToPosition(c))
  }

  def castLight(wPos: IPoint): Unit = {
    for {litSquare@IPoint(x, y) <- watchman.getTorchRange(wPos)
         if positionWithinBoundingBox(litSquare)} {
      val cell = getCell(litSquare)
      if (cell.canStep &&
        !cell.isIlluminated &&
        squareIsVisible(wPos, litSquare)) {
        cell.shedLight()
      }
    }
  }

}
