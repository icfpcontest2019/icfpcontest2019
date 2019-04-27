package lambda.util

import akka.actor.ActorRef

/**
  * @author Ilya Sergey
  */
abstract class ScenarioMessage(val lines: Seq[String]) {
  def getValidator(sRoot: String, tname: String, logActor: ActorRef): SolutionValidator

  def toSubmissionType: SubmissionType
}

object ScenarioMessage {
  def unapply(sm: ScenarioMessage): Option[Seq[String]] = sm match {
    case sm: ScenarioMessage => Some(sm.lines)
    case _ => None
  }
}