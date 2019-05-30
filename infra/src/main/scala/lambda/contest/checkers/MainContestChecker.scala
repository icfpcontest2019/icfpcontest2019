package lambda.contest.checkers

import lambda.contest.generators.runners.matrices.TaskMatrixConverter
import lambda.solvers.SimpleSolver

/**
  *
  * Main checker for the contest
  *
  * @author Ilya Sergey
  */
object MainContestChecker {

  val TOOLNAME = "ICFP Contest 2019 Checker"
  val SCRIPTNAME = "checker"
  val VERSION = "1.0"
  val VERSION_STRING = s"v$VERSION"
  val defaultProblems = "."
  val defaultTeams = "."

  object CheckerMode extends Enumeration {
    type CheckerMode = Value
    val Team, Solver, MatrixConverter, Chain, Bad = Value
  }

  private val mainParser = new scopt.OptionParser[CheckerMode.Value](SCRIPTNAME) {
    head(TOOLNAME, VERSION_STRING)
    arg[String]("mode")
      .required()
      .action { (x, c) =>
      x match {
        case "team" => CheckerMode.Team
        case "solver" => CheckerMode.Solver
        case "chain" => CheckerMode.Chain
        case "matrix" => CheckerMode.MatrixConverter
      }
    }.text(s"a mode to run the checker: individual team (team), solver (solver), or chain processing (chain).")

    help("help").text("prints this usage text")
  }

  def main(args: Array[String]): Unit = {
    val mode = if (args.length < 1) args else Array(args(0))
    
    mainParser.parse(args = mode, init = CheckerMode.Bad) match {
      case Some(m: CheckerMode.Value) if m != CheckerMode.Bad =>
        val newArgs = new Array[String](args.length - 1)
        for (i <- 1 until args.length) {
          newArgs(i - 1) = args(i)
        }
        m match {
          case CheckerMode.Team => IndividualGrader.main(newArgs)
          case CheckerMode.Solver => SimpleSolver.main(newArgs)
          case CheckerMode.Chain => ChainEvaluator.main(newArgs)
          case CheckerMode.MatrixConverter => TaskMatrixConverter.main(newArgs)
          case _ => System.err.println("Unknown checker mode")
        }
      case _ => 
        //System.err.println("Provided arguments for the checker are malformed.")
    }

  }


}



