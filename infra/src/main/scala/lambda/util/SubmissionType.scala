package lambda.util

/**
  * @author Ilya Sergey
  */
trait SubmissionType {
  def toSolution(content: Seq[String]): ScenarioMessage
  def toWebPath: String
  def toDescription: String
  def toSolutionFolder: String
  def toScorePath: String
}
