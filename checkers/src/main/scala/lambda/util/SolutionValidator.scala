package lambda.util

import java.util.{Calendar, Date}

import akka.actor.ActorRef
import lambda.geometry.Point2D
import lambda.util.project.ScenarioInstance
import lambda.geometry.Point2D
import lambda.util.project.ProjectConstants.{dateFormat, dumpPrefix, msgPrefix}
import lambda.util.project.ScenarioInstance

/**
  * @author Ilya Sergey
  */
trait SolutionValidator extends SolutionReportUtils {

  val sRoot: String
  val tname: String
  val logActor: ActorRef
  val scenarioInstance : ScenarioInstance

  type OkSolutionType
  type Solution

  def processSolution(lines: Seq[String]): (String, Option[Seq[OkSolutionType]])

  def dumpResultsToFiles(submissionTime: Date,
                         succOpt: Option[Seq[OkSolutionType]],
                         lines: Seq[String], msg: String): Unit

  def dumpSubmission(submissionTime: Date, st: SubmissionType,
                     lines: Seq[String]): Unit = {
    val date = dateFormat.format(submissionTime)
    val out = (Seq(date) ++ lines).mkString("\n")

    scenarioInstance.writeIntoResultFile(sRoot, tname, st, out, dumpPrefix)
  }

  def dumpMessage(st: SubmissionType, msg: String, submissionTime: Date): Unit = {
    // Outpu both dates: submission and when was processed (since the discrepancy might be non-trivial)
    val date = dateFormat.format(submissionTime)
    val now = dateFormat.format(Calendar.getInstance().getTime)
    val out = Seq(date, now, msg).mkString("\n")
    scenarioInstance.writeIntoResultFile(sRoot, tname, st, out, msgPrefix)
  }

  def epsMove(vs: Seq[Point2D])(g: Point2D): Point2D = {
    val res = vs.find(p => p =~= g)
    if (res.isDefined) res.get else g
  }
}
