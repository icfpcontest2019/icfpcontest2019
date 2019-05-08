package lambda.util.stats

import lambda.geometry.GeometryParsers
import lambda.util.project.{BadSetupException, ScenarioInstance}
import lambda.util.{ScenarioUtils, SubmissionType}

import scala.io.Source
import scala.xml.Elem

/**
  * @author Ilya Sergey
  */

abstract class StatisticsRenderer extends StatisticsUtils[String, String] {

  val SInstance : ScenarioInstance
  val SType : SubmissionType

  object ScoreParser extends GeometryParsers {
      def line: Parser[(Int, Double)] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
        case pNum ~ score => (pNum.toInt, score.toDouble)
      }

      def apply(s: String): ParseResult[(Int, Double)] = parseAll(line, s)
    }

  private type TeamResult = Map[Int, Option[Double]]
  private val rankingPrecision = 3

  implicit val ord = new Ordering[TeamRow] {
    override def compare(x: TeamRow, y: TeamRow): Int =
      if (x.rank < y.rank)  -1
      else if (x.rank > y.rank) 1
      else x.team.compare(y.team)
  }


  type AllTeamsResults = Map[String, (String, TeamResult)]

  /**
    * Get a table representation as a sorted sequence of rows
    */
  def buildRankingTable(sDir: String): Seq[TeamRow] = {
    val teamToTimeResults: AllTeamsResults = computeAllTeamResults(sDir)

    val orderScoreByIncreasing = true
    val rankToTeams: Map[Int, Seq[String]] = rankTeams(teamToTimeResults, rankingPrecision, orderScoreByIncreasing)
    // TODO: build entire rows

    // Mapping teams back to ranks
    val rankTeamsMap: Map[String, Int] =
      (for ((rk, ts) <- rankToTeams.toSeq) yield ts.map(t => (t, rk))).flatten.toMap

    assert(rankTeamsMap.size == teamToTimeResults.size)

    val rows = for ((team, (time, results)) <- teamToTimeResults) yield {
      assert(rankTeamsMap.isDefinedAt(team))
      // TODO: Can throw an exception!!!
      val rank = rankTeamsMap(team)
      // How many problems done
      val done = results.count(_._2.isDefined)
      val resultStrings: Seq[Elem] = processResults(results)
      TeamRow(team, rank, time, done, resultStrings)
    }

    rows.toSeq.sorted
  }


  def processResults(results: TeamResult): Seq[Elem] = {
    val na = <font color="red">N/A</font>
    for ((p, res) <- results.toSeq.sortBy(_._1)) yield {
      if (res.isDefined) {
        val d : Double = res.get
        val dRounded = roundToPrecision(d, 3)
        <font color="black">{dRounded}</font>
      }
      else <font color="red">N/A</font>
    }
  }

  /**
    * Results for all teams
    */
  private def computeAllTeamResults(sDir: String): AllTeamsResults = {
    val teams = ScenarioUtils.getTeams(sDir)
    val keys = SInstance.getProblemMap(sDir).keys
    (for {
      team <- teams
      tres = getFullResultsForATeam(sDir, team, keys)
    } yield team -> tres).toMap
  }


  def getFullResultsForATeam[P](sDirPath: String, tName: String,
                                keys: Iterable[Int]): (String, TeamResult) = {
    val res = getLastTeamResults(sDirPath, tName)
    // Return default map
    if (res.isEmpty) {
      return ("N/A", (for (p <- keys) yield p -> None).toMap)
    }
    val (time, rmap) = res.get
    val gmap = (for (p <- keys) yield p -> (if (rmap.isDefinedAt(p)) Some(rmap(p)) else None)).toMap
    (time, gmap)
  }

  /**
    * Get a partial result table for a specific's team last submission.
    * Absent solutions are simply not present.
    *
    * The String part of the result is the time
    */
  private def getLastTeamResults(sDirPath: String, tName: String): Option[(String, Map[Int, Double])] = {
    val fPath = SInstance.getLastResultPath(sDirPath, tName, SType)
    // No last result
    if (fPath.isEmpty) return None
    try {
      val lines = Source.fromFile(fPath.get).getLines.toSeq.filter(_.trim.nonEmpty)
      if (lines.isEmpty) throw BadSetupException(s"File $fPath is empty!")
      val time = lines.head
      // results for this team
      val results = lines.tail.map(ScoreParser(_).get).toMap
      Some((time, results))
    } catch {
      case e: Throwable =>
        val msg = e.getMessage
        throw BadSetupException(s"Cannot read/parse the input for the file $fPath:\n $msg")
    }
  }
}

case class TeamRow(team: String, rank: Int, time: String, done: Int, results: Seq[Elem])


