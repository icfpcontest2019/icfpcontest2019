package lambda.contest.checkers

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
    val Team, Batch, Chain, Bad = Value
  }

  private val mainParser = new scopt.OptionParser[CheckerMode.Value](SCRIPTNAME) {
    head(TOOLNAME, VERSION_STRING)
    arg[String]("mode")
      .required()
      .action { (x, c) =>
      x match {
        case "team" => CheckerMode.Team
        case "batch" => CheckerMode.Batch
        case "chain" => CheckerMode.Chain
      }
    }.text(s"a mode to run the checker: individual team (team), batch-grading (batch), or chain processing (chain).")

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
          case CheckerMode.Batch => BatchGrader.main(newArgs)
          case CheckerMode.Chain => ChainEvaluator.main(newArgs)
          case _ => System.err.println("Malformed checker mode.")
        }
      case _ => 
        //System.err.println("Provided arguments for the checker are malformed.")
    }

  }


}



