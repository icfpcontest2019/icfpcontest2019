package lambda.util

import scala.xml.Elem

/**
  * @author Ilya Sergey
  */

object InputUtils {

  val FILE_TOO_LARGE : Elem = <p>The file is too large!</p>

  val TOO_FEW_LINES_MSG: Elem =
    <p>Your input file should contain at least two lines: with the group name and password.</p>

  val NO_SUCH_TEAM_MSG: (String) => Elem = (tname: String) => <p>There is no team with the name
    <b>
      {tname}
    </b>
    .</p>

  val NO_PW_FOR_TEAM_MSG: (String) => Elem = (tname: String) => <p>No password set up for the team
    <b>
      {tname}
    </b>
    .</p>

  val WRONG_PASS_MSG: (String) => Elem = (tname: String) => <p>Wrong password for the team
    <b>
      {tname}
    </b>
  </p>

  val OK_MSG: Elem = <p>OK</p>

  def splitStringIntoLines(s: String): Seq[String] = s.split("\\r?\\n+").toSeq.filter(!_.trim.isEmpty)

  /**
    *
    * @param sDirPath the path to the scenario instance folder
    * @param lines input file lines
    * @return
    */
  def preValidateInputText(sDirPath: String, lines: Seq[String]): (Boolean, Elem, Option[(String, Seq[String])]) = {
    if (lines.length < 2) {
      return (false, TOO_FEW_LINES_MSG, None)
    }
    val tname = lines.head
    val tpass = lines(1)

    if (!BasicUtils.teamFolderExists(sDirPath, tname)) {
      return (false, NO_SUCH_TEAM_MSG(tname), None)
    }

    val pwr = BasicUtils.getTeamPassword(sDirPath, tname)
    if (pwr.isEmpty) {
      return (false, NO_PW_FOR_TEAM_MSG(tname), None)
    }

    val pw = pwr.get
    if (!pw.equals(tpass)) {
      return (false, WRONG_PASS_MSG(tname), None)
    }

    (true, OK_MSG, Some((tname, lines.drop(2))))
  }

}
