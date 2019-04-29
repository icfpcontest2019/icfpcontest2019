package lambda.util.email

import EmailUtils.Mail
import lambda.util.ScenarioUtils

/**
  * @author Ilya Sergey
  */
object MailFeedbackToEveryone {

  def main(args: Array[String]) {
    val sRoot = args(0)
    val tss = getTeamsEmailsAndfeedback(sRoot)
    for ((tname, pass, emails) <- tss) {
      sendEmailWithFeedback(tname, pass, emails)
    }
  }

  def sendEmailWithFeedback(tname: String, text: String, emails: Seq[String]): Unit = {
    if (emails.isEmpty) return
    val body =
      s"""Dear participants of the Scenario Week 4.
        |
        |Please, find below the grades for the week and feedback on the implementation report for your team "$tname".
        |
        |$text
        |
        |Kind regards,
        |Scenario Week Team
      """.stripMargin

    val subj = s"[$tname] Grades and Feedback for Scenario Week 4"
    val mail = Mail(to = emails, subject = subj, message = body, bcc = Seq("ilya.sergey@gmail.com"))
    EmailUtils.send(mail)
    println(s"Mailed to '$tname':\n${emails.mkString("\n")}")
    println()
  }

  def getTeamsEmailsAndfeedback(sRoot: String): Seq[(String, String, Seq[String])] = {
    val ts = ScenarioUtils.getTeams(sRoot)
    val res = for (tname <- ts) yield {
      val feedback = ScenarioUtils.getTeamFeedback(sRoot, tname).getOrElse("<No feedback>")
      val emails = ScenarioUtils.getTeamEmails(sRoot, tname).sorted
      (tname, feedback, emails)
    }
    res
  }
}
