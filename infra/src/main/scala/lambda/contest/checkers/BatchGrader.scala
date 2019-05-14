package lambda.contest.checkers

import lambda.contest.checkers.MainContestChecker.{SCRIPTNAME, TOOLNAME}

/**
  * @author Ilya Sergey
  */
object BatchGrader extends ContestGrader {

  def main(args: Array[String]): Unit = {
    val configOpt = handleInput(args)
    if (configOpt.isEmpty) {
      return
    }

    val config = configOpt.get
    // TODO: read tasks
    
    // TODO: Get the required team folder and get the latest solution


    // TODO: Grade the submissions -- check IndividualGrader for examples

  }

  /* --------------------------------------------------------- */
  /*                       Parsing input flags                 */
  /* --------------------------------------------------------- */

  case class BatchGraderConfig(problemPath: String = LOCAL_DIR,
                               teamPath: String = LOCAL_DIR,
                               outPath: String = s"LOCAL_DIR/results.csv",
                               override val verbose: Boolean = false)
    extends GraderConfig(verbose)

  type MyConfig = BatchGraderConfig

  override val defaultConfig = BatchGraderConfig()

  protected val flagParser = new scopt.OptionParser[BatchGraderConfig](SCRIPTNAME) {
    head(TOOLNAME, MainContestChecker.VERSION_STRING)

    opt[String]('p', "problems")
      .required()
      .valueName("<problemsPath>")
      .action { (x, c) =>
        c.copy(problemPath = x)
      }.text("A path to the folder with problems ")

    opt[String]('t', "teamFolder")
      .required()
      .valueName("<teamFolderPath>")
      .action { (x, c) =>
        c.copy(teamPath = x)
      }.text("A path to the folder with solutions of a particular team")

    opt[String]('o', "out")
      .required()
      .valueName("<outputFilePath>")
      .action { (x, c) =>
        c.copy(outPath = x)
      }.text("An output file")

    opt[Boolean]('v', "verbose")
      .valueName("<verbose>")
      .action { (x, c) =>
        c.copy(verbose = true)
      }.text("Verbose output")

    help("help").text("prints this usage text")
  }


}
