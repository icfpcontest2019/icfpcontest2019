package lambda.util.project

import java.util.Date

/**
  * Util functions for the Scenario Project
  *
  * @author Ilya Sergey
  */

object ProjectConstants {
  val homePath = "home"
  val testUserName = "bender"
  val testPassword = "bitemyshinymetalass"

  val dateFormat = new java.text.SimpleDateFormat("HH:mm:ss, dd MMM yyyy")

  val tasksDir = "tasks"
  val teamsDir = "teams"
  val passwordFile = "password"
  val emailsFile = "emails"
  val feedbackFile = "feedback.txt"
  val configFile = "config"

  val resultsPrefix = "results."
  val dumpPrefix = "dump."
  val msgPrefix = "msg."
  val tmpSuffix = "tmp"


  val DEFAULT_HOST: String = "127.0.0.1"
  val DEFAULT_PORT: Int = 8083
  val DEFAULT_REPORT_MAIL: Option[String] = None
  val DEFAULT_STOP_DATE: Option[Date] = None

  val commentToken = "#"
  val hostKey = "host"
  val portKey = "port"
  val reportKey = "reportMail"
  val stopDate = "stopDate"
}
