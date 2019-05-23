package lambda.solvers

import java.io.File

import lambda.contest.ContestConstants.Action
import lambda.contest.checkers.GraderUtils._
import lambda.contest.checkers.{TaskExecution, TaskMatrix}
import lambda.contest.parsers.ContestSolutionParser
import lambda.contest.{Cell, ContestException, Watchman}
import lambda.geometry.integer.IPoint
import lambda.geometry.integer.IntersectionUtils.cellsIntersectedByViewSegment
import lambda.util.FileUtil
import lambda.util.FileUtil.intAs3CharString

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object SimpleSolver {

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("Noe task file provided!")
      return
    }
    val baseDir = new File(args(0))
    val solutionDir = new File(args(1))
    assert(baseDir.isDirectory)
    if (!solutionDir.exists()) {
      solutionDir.mkdirs()
    }

    val dirs = baseDir.listFiles().toList.filter(_.isDirectory).sortBy(_.getName)
    for {d <- dirs
         dPath = d.getAbsolutePath
         nums = getTaskNumbers(dPath)
         taskNum <- nums
         res = readOneTask(dPath, taskNum)
         if res.isDefined} {
      val (matrix, dx, dy, init) = res.get
      print(s"Task $taskNum (${dx}x$dy): ")
      val t0 = System.currentTimeMillis()
      val solString = solveTask(matrix, dx, dy, init)
      val t1 = System.currentTimeMillis()
      val tsec1 = (t1 - t0).toDouble / 1000
      print(s"solution size: ${solString.length} ($tsec1 sec); checking: ")

      val t2 = System.currentTimeMillis()

      val solution = ContestSolutionParser(solString).get

      // Now checking, with a fresh matrix

      val (matrix1, _, _, _) = readOneTask(dPath, taskNum).get

      // TODO: fix bug with dark corners

      val state = TaskExecution.createState(matrix1, dx, dy, init, solution, Nil)
      val checkingResult = try {
        state.evalSolution()
        match {
          case Some(value) => "OK"
          case None => "Fail"
        }
      } catch {
        case ContestException(loc, _) =>
          val msg = s"Failure: $loc."
          Right(msg)
        case _: Throwable => "Exception"
      }
      val t3 = System.currentTimeMillis()
      val tsec2 = (t3 - t2).toDouble / 1000
      println(s"$checkingResult ($tsec2 sec)")
      
      writeSolutionToFile(solutionDir, taskNum, solString)
    }


  }
  
  def writeSolutionToFile(solutionDir: File, num: Int, sol: String) = {
    val fName = s"$PROBLEM_PREFIX-${intAs3CharString(num)}${SOLUTION_EXT}"
    val path = s"$solutionDir/$fName"
    FileUtil.writeToNewFile(path, sol)
    
  }

  def solveTask(matrix: TaskMatrix, xmax: Int, ymax: Int, initPos: IPoint): String = {
    val problem = ContestProblem(matrix, xmax, ymax, initPos)
    // Initialize
    problem.doStuff()
    while (problem.hasMoreDarkNeighbours) {
      problem.step()
    }
    problem.getFinalPath.map(l => l.map(_.pp).mkString).mkString("#")
  }


  case class ContestProblem(matrix: TaskMatrix, xmax: Int, ymax: Int, private var currentPos: IPoint) {


    val watchman = new Watchman()

    val pathUnderConstruction: mutable.Queue[IPoint] = new mutable.Queue[IPoint]()
    var darkNeighbours: collection.mutable.HashSet[IPoint] = new mutable.HashSet[IPoint]()

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
      darkNeighbours = darkNeighbours.filterNot(p => getCell(p).isIlluminated)

      val litCells = getLightableNeighbourCells(currentPos).toSet
      val nextMoves = litCells.flatMap(c => getNextMoves(c))
      val dark = nextMoves.filter(n => !getCell(n).isIlluminated)
      
      dark.foreach(darkNeighbours.add(_))
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
    private def getNextMovesWithPriorities(p: IPoint): List[(IPoint, Int)] = {
      getNextMoves(p).zipWithIndex
    }

    private def getNextMoves(p: IPoint) = {
      val IPoint(x, y) = p
      List(IPoint(x + 1, y),
        IPoint(x, y + 1),
        IPoint(x - 1, y),
        IPoint(x, y - 1)).filter(canStepToPosition)
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
      val nextMove: IPoint = getNextMovesWithPriorities(currentPos)
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
      while (queue.nonEmpty && !darkNeighbours.forall(n => marked.contains(n))) {
        val pos = queue.dequeue()
        for (n <- getNextMoves(pos)
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

      assert(SolverUtils.getPairs(path).forall({ case (a, b) =>
        val diff = a - b
        math.abs(diff.x) + math.abs(diff.y) == 1
      }))

      path
    }


    private def executeSearchPath(path: List[IPoint]): Unit = {

    }

    def getFinalPath: List[List[Action]] =
      SolverUtils.convertPointsToMoves(pathUnderConstruction.toList)


  }

}
