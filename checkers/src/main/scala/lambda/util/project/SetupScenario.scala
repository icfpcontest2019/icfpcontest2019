package lambda.util.project

/**
  * Creating a new scenario instance
  */
case class BadSetupException(msg: String) extends Exception

trait SetupScenario {

  val instance: ScenarioInstance

  import instance._

  /**
    * Create new scenario structure
    * args(0) -- base folder for the scenarios, e.g., ./instances
    * args(1) -- scenario name, e.g., test
    */
  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Not enough arguments for creating a scenario structure.\n" +
          "Please, provide a base path and a scenario name. For example:\n" +
          "./instances/moveandtag/ test")
      return
    }

    val basePath = args(0)
    val scenarioName = args(1)

    var deleteOld: Boolean = false
    if (args.length >= 3) {
      deleteOld = java.lang.Boolean.parseBoolean(args(2))
    }

    var testEmail: Option[String] = None
    if (args.length >= 4) {
      testEmail = Some(args(3))
    }

    val res = populateInstance(basePath, scenarioName, deleteOld, testEmail)
  }

}
