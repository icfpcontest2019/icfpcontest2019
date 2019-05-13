package lambda.contest.checkers

import java.io.File

import lambda.contest.checkers.MainContestChecker.{SCRIPTNAME, TOOLNAME}

/**
  * @author Ilya Sergey
  */
object IndividualGrader extends ContestGrader {
  
  def main(args: Array[String]): Unit = {

    val configOpt = handleInput(args)
    if (configOpt.isEmpty) {
      return
    } 
    
    val config = configOpt.get
    // Get all tasks
    val taskMap = readTasks(config.problemPath)
    
    // Get all solutions (with boosters) from the folder 
    val solutionMap = readSolutionsAndBoosters(config.solutionPath)
    val solutionFolder = new File(config.solutionPath).getName
    
    // Grade solutions
    val gradeMap = gradeSolutions(solutionFolder, taskMap, solutionMap, config)

    if (config.verbose) {
      println()
      println("Results:")

      for (i <- gradeMap.keys.toList.sorted) {
        val res = gradeMap(i)
        val steps = if (res.isDefined) res.get else 0
        val status = if (res.isDefined) "Ok" else "Failed"
        println(s"$i, $steps, $status")
      }
    }
    
    // Writing to CSV
    writeGradesToCSV(gradeMap, config.outPath)
    
    
    println("Grading complete")
    
    

  }


  /* --------------------------------------------------------- */
  /*                       Parsing input flags                 */
  /* --------------------------------------------------------- */

  case class IndividualGraderConfig(problemPath: String = LOCAL_DIR,
                                    solutionPath: String = LOCAL_DIR,
                                    outPath: String = s"$LOCAL_DIR/results.csv",
                                    override val verbose: Boolean = false)
    extends GraderConfig(verbose)

  type MyConfig = IndividualGraderConfig

  override val defaultConfig = IndividualGraderConfig()
  


  protected val flagParser = new scopt.OptionParser[IndividualGraderConfig](SCRIPTNAME) {
    head(TOOLNAME, MainContestChecker.VERSION_STRING)

    opt[String]('p', "problems")
      .required()
      .valueName("<problemsPath>")
      .action { (x, c) =>
        c.copy(problemPath = x)
      }.text("A path to the folder with problems ")

    opt[String]('s', "solutions")
      .required()
      .valueName("<solutionsPath>")
      .action { (x, c) =>
        c.copy(solutionPath = x)
      }.text("A path to a particular folder with solutions")

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




