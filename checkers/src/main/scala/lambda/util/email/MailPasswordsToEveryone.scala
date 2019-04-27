package lambda.util.email

import EmailUtils.Mail
import lambda.util.BasicUtils

/**
  * @author Ilya Sergey
  */
object MailPasswordsToEveryone {

  def main(args: Array[String]) {
    val sRoot = args(0)
    val tss = getTeamsPassEmails(sRoot)
    for ((tname, pass, emails) <- tss) {
      sendEmailWithPassword(tname, pass, emails)
    }
  }

  def sendEmailWithPassword(tname: String, pass: String, emails: Seq[String]): Unit = {
    if (emails.isEmpty) return
    val body =
      s"""Dear participant of the Scenario Week 2.
        |
        |You have been assigned to the team "$tname".
        |
        |Your team's password for submitting solutions is:
        |
        |$pass
        |
        |Good luck!
        |
        |Scenario Week Team
      """.stripMargin

    val subj = s"[$tname] Welcome to Scenario Week 2"
    val mybcc = Seq("ilya.sergey@gmail.com")
    val mail = Mail(to = emails, subject = subj, message = body, bcc = mybcc)
    EmailUtils.send(mail)
    println(s"Mailed to '$tname':\n${emails.mkString("\n")}")
    println()
  }

  def getTeamsPassEmails(sRoot: String): Seq[(String, String, Seq[String])] = {
    val ts = BasicUtils.getTeams(sRoot)
    val res = for (tname <- ts) yield {
      val pass = BasicUtils.getTeamPassword(sRoot, tname).get
      val emails = BasicUtils.getTeamEmails(sRoot, tname).sorted
      (tname, pass, emails)
    }
    res
  }
}
