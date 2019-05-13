package lambda.contest.checkers

import lambda.contest.ContestException
import lambda.contest.checkers.MainContestChecker.{SCRIPTNAME, TOOLNAME}

/**
  * @author Ilya Sergey
  */
object IndividualChecker {

  def main(args: Array[String]): Unit = {
    val config = handleInput(args)
    println(s"Problems: ${config.problemPath}")
    println(s"Teams: ${config.teamsPath}")


    // TODO: Load all task descriptions

    // TODO: Load individual team solutions

  }

  private val teamModeParser = new  scopt.OptionParser[TeamCheckerConfig](SCRIPTNAME) {
    head(TOOLNAME, MainContestChecker.VERSION_STRING)

    opt[String]('p',"problems").action { (x, c) =>
      c.copy(problemPath = x)
    }.text("A path to the folder with problems ")

    opt[String]('s',"solutions").action { (x, c) =>
      c.copy(teamsPath = x)
    }.text("A path to the folder with solutions of a particular team")
    help("help").text("prints this usage text")
  }

  def handleInput(paramString: Array[String]): TeamCheckerConfig = {
    val newConfig = TeamCheckerConfig(".", ".")
    teamModeParser.parse(paramString, newConfig) match {
      case Some(synConfig) => synConfig
      case None => throw ContestException("Bad argument format.")
    }
  }
  
}

case class TeamCheckerConfig(problemPath: String, teamsPath: String)



