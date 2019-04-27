package lambda.util.project

import java.util.Date

/**
  * @author Ilya Sergey
  */
case class ProjectConfig(host: String = ProjectConstants.DEFAULT_HOST,
                         port: Int = ProjectConstants.DEFAULT_PORT,
                         reportMail: Option[String] = ProjectConstants.DEFAULT_REPORT_MAIL,
                         stopDate: Option[Date] = ProjectConstants.DEFAULT_STOP_DATE,
                         root: String)
