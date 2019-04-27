package lambda.misc.artgallery

import lambda.misc.artgallery.ArtGalleryInstance._
import lambda.util.BasicUtils
import lambda.util.project.BadSetupException

import scala.io.Source
import scala.xml.{Elem, Node, NodeSeq}

/**
  * @author Ilya Sergey
  */

object CheckStatisticsUtils  {

  type PerPolCheck = Seq[(Int, Boolean)]

  /**
    * Returns the mapping from polygons to the set of nodes
    */
  private def computeCheckPolygonInfos(sRoot: String): Seq[Int] = {
    val map = getProblemMap(sRoot)
    map.keys.toSeq.sorted
  }

  private def getLastCheckResults(sDirPath: String, tName: String): Option[(String, Seq[Int])] = {
    val fPath = getLastResultPath(sDirPath, tName, CheckSubmission)
    if (fPath.isEmpty) return Some(("N/A", Seq.empty))
    try {
      val lines = Source.fromFile(fPath.get).getLines.toSeq.filter(_.trim.nonEmpty)
      if (lines.isEmpty) throw new BadSetupException(s"File $fPath is empty!")
      val time = lines.head
      // results for this team
      val results: Seq[Int] = lines.tail.map(CheckResultParser(_).get._1)
      Some((time, results))
    } catch {
      case e: Throwable =>
        val msg = e.getMessage
        throw BadSetupException(s"Cannot read/parse the input for the file $fPath:\n $msg")
    }
  }


  def getCheckResultsForATeam(sDirPath: String, tName: String, pseq: Seq[Int]): Option[(String, PerPolCheck)] = {
    val res = getLastCheckResults(sDirPath, tName)
    if (res.isEmpty) return None
    val (time, rseq) = res.get
    val sorted = (for (p <- pseq.toSeq) yield (p, rseq.contains(p))).sortWith((x, y) => x._1 <= y._1)
    Some(time, sorted)
  }

  type AllTeamsCheckResults = Map[String, (String, PerPolCheck)]

  private def computeAllTeamCheckResults(sDir: String): AllTeamsCheckResults = {
    val teams = BasicUtils.getTeams(sDir)
    val pmap: Seq[Int] = computeCheckPolygonInfos(sDir)
    val opts = (for {
      team <- teams
      tres = getCheckResultsForATeam(sDir, team, pmap)
    } yield team -> tres).toMap

    val fpMap: PerPolCheck = pmap.map((_, false))
    opts.mapValues(e => if (e.isDefined) e.get else ("N/A", fpMap))

  }

  private def computeFirstCheckRow(pseq: Seq[Int]): Node = {
    val fstCells: NodeSeq = Seq(<td><b>Team/Problem</b></td>) ++
        Seq(<td align="center"><b>Last submitted</b></td>) ++
        Seq(<td align="center"><b>Done</b></td>) ++
        pseq.map(e => <td align="center"><b>{e.toString}</b></td>)
    <tr>{fstCells}</tr>
  }

  private def computeTeamCheckRow(tstat: (String, (String, PerPolCheck))): Node = {
    val (tname, (time, pstat)) = tstat
    val done = pstat.count(_._2)
    val ccells = pstat.map(e => <td align="center">{
      if (e._2) <font color="blue">OK</font> else <font color="red">N/A</font>
      }</td>)
    val tRow= Seq(<td>{tname}</td>, <td align="center">{time}</td>, <td align="center">{done}</td>) ++ ccells
    <tr>{tRow}</tr>
  }


  def renderCheckStatistics(sDir: String): Elem = {
    try {
      val allTeamsResults = computeAllTeamCheckResults(sDir)
      val pseq = computeCheckPolygonInfos(sDir).toSeq.sorted
      val firstRow = computeFirstCheckRow(pseq)
      val tstats = allTeamsResults.toSeq.sortBy(_._1)

      val tRows = tstats.map(computeTeamCheckRow)
      val allRows = Seq(firstRow) ++ tRows
      <table border="1" cellpadding="4">
        {allRows}
      </table>
    } catch {
      case t: Throwable =>
        //TODO provide a logger for this
        println(t.getMessage)
        t.printStackTrace()
        <p>Oops, something went wrong. Please, contact the organizers and try again.</p>
    }
  }
}


object GuardsStatisticsUtils {

  private type PolInfo = Map[Int, Int]
  /**
    * Returns the mapping from polygons to the set of nodes
    */
  private def computeGuardPolygonInfos(sRoot: String): PolInfo = {
    val map = getGuardsPolygonMap(sRoot)
    val res = (for (k <- map.keys; n = map(k).vertices.size) yield k -> n).toMap
    res
  }

  /**
    * returns time and last map of results for the appropriate guards
    */
  private def getLastGuardsResults(sDirPath: String, tName: String): Option[(String, PolInfo)] = {
    val fPath = getLastResultPath(sDirPath, tName, GuardSubmission)
    if (fPath.isEmpty) return Some(("N/A", Map.empty: PolInfo))
    try {
      val lines = Source.fromFile(fPath.get).getLines.toSeq.filter(_.trim.nonEmpty)
      if (lines.isEmpty) throw new BadSetupException(s"File $fPath is empty!")
      val time = lines.head
      // results for this team
      val results: PolInfo = lines.tail.map(GuardResultParser(_).get).toMap
      Some((time, results))
    } catch {
      case e: Throwable =>
        val msg = e.getMessage
        throw BadSetupException(s"Cannot read/parse the input for the file $fPath:\n $msg")
    }
  }

  /**
    * Mapping from a polygon number to the number of guards
    */
  type PerPolGuards = Seq[(Int, Option[Int])]

  def getGuardResultsForATeam(sDirPath: String, tName: String, pmap: PolInfo): Option[(String, PerPolGuards)] = {
    val res = getLastGuardsResults(sDirPath, tName)
    if (res.isEmpty) return None
    val (time, rmap) = res.get
    val gmap = (for (p <- pmap.keys) yield p -> (if (rmap.isDefinedAt(p)) Some(rmap(p)) else None)).toMap
    val sorted = gmap.toSeq.sortWith((x, y) => x._1 <= y._1)
    Some(time, sorted)
  }

  type AllTeamsResults = Map[String, Option[(String, PerPolGuards)]]

  private def computeAllTeamGuardsResults(sDir: String): AllTeamsResults = {
    val teams = BasicUtils.getTeams(sDir)
    val pmap = computeGuardPolygonInfos(sDir)
    (for {
      team <- teams
      tres = getGuardResultsForATeam(sDir, team, pmap)
    } yield team -> tres).toMap
  }

  /**
    * Returns a statistics per-team and polygon info
    * Components of the result:
    *
    * 1. Seq(Team, TimeSubmitted, Results)
    * 2. Polygons and the numbers of their vertices
    */
  def computeGuardsStatistics(sDir: String,
                              allTeamsResults: AllTeamsResults): (Seq[(String, String, PerPolGuards)], Seq[(Int, Int)]) = {
    val pmap = computeGuardPolygonInfos(sDir)
    val stat = for {
      team <- allTeamsResults.keys
      tres = allTeamsResults(team)
      if tres.isDefined
      (time, res) = tres.get
    } yield (team, time, res)

    val res = stat.toSeq.sortWith((x, y) => x._1 <= y._1)
    val pseq = pmap.toSeq.sortWith((x, y) => x._1 <= y._1)
    (res, pseq)
  }

  private def computeFirstRow(pseq: Seq[(Int, Int)]): Node = {
    val fstCells: NodeSeq = Seq(<td><b>Team/Gallery (N)</b></td>) ++
        Seq(<td align="center"><b>Last submitted</b></td>) ++
        Seq(<td align="center"><b>Rank</b></td>) ++
        Seq(<td align="center"><b>Done</b></td>) ++
        pseq.map(e => <td align="center"><b>{e._1.toString} ({e._2.toString})</b></td>)
    <tr>{fstCells}</tr>
  }

  private def computeTeamGuardRow(tstat: (String, Int, String, PerPolGuards)): Node = {
    val (tname, rk, time, pstat) = tstat
    val done = pstat.count(_._2.isDefined)
    val gnums = pstat.map(e => <td align="center">
      {if (e._2.isDefined) e._2.get else <font color="red">N/A</font>}
    </td>)
    val tRow = Seq(<td>{tname}</td>,
      <td align="center">{time}</td>,
      <td align="center">{renderRank(rk)}</td>,
      <td align="center">{done}</td>) ++ gnums
    <tr>{tRow}</tr>
  }

  def renderRank(rank: Any): Any = {
    if (!rank.isInstanceOf[Int]) return rank
    val rk = rank.asInstanceOf[Int]
    val color =
      if (0 < rk && rk <= 3) "blue"
      //      else if (rk > 3 && rk <= 5) "DarkCyan"
      //      else if (rk > 5 && rk <= 7) "MediumSeaGreen"
      else "Black"
    val rkColored = <font color={color}>
      {rank}
    </font>
    if (0 < rk && rk <= 3) <b>
      {rkColored}
    </b>
    //    else if (rk > 3 && rk <= 5) <i>{rkColored}</i>
    else rkColored
  }

  def renderGuardsStatistics(sDir: String): Elem = {
    try {
      val allTeamsResults = computeAllTeamGuardsResults(sDir)
      val (tstats, pseq) = computeGuardsStatistics(sDir, allTeamsResults)
      val firstRow = computeFirstRow(pseq)

      val ranking = rankTeams(sDir, allTeamsResults)
      val tstatWRank = tstats.map({
        case (team, time, pp) => (team, ranking(team), time, pp)
      })
      // sort by rank and then by name
      val sorted = tstatWRank.groupBy(_._2).toSeq.sortBy(_._1).
          flatMap { case (r, s) => s.sortBy(_._1) }
      val tRows = sorted.map(computeTeamGuardRow)

      val allRows = Seq(firstRow) ++ tRows
      <table border="1" cellpadding="4">
        {allRows}
      </table>
    } catch {
      case t: Throwable =>
        //TODO provide a logger for this
        println(t.getMessage)
        t.printStackTrace()
        <p>Oops, something went wrong. Please, contact the organizers and try again.</p>
    }
  }

  /**
    * Returns a map from each polygon to the ranking of the teams with respect to it
    * and also to the number of groups (i.e., the worst ranking boundary)
    */
  def rankForPolygons(sDir: String, allTeamsResults: AllTeamsResults): Map[Int, (Map[String, Int], Int)] = {
    val pmap = computeGuardPolygonInfos(sDir)

    val ptrs: Seq[(Int, (String, Int))] = for {
      (p, pvs) <- pmap.toSeq
      team <- allTeamsResults.keys
      tres = allTeamsResults(team)
      // number of nodes
      tNum = if (tres.isEmpty) pvs
      else {
        val res = tres.get._2.toMap
        // number of guards for p, delivered by this team
        if (res.isDefinedAt(p) && res(p).isDefined) math.min(res(p).get, pvs) else pvs
      }
    } yield (p, (team, tNum))

    val polToTeamRes: Map[Int, Seq[(String, Int)]] = ptrs.groupBy { case (p, ts) => p }.mapValues(s => s.map(_._2))

    // For each polygon, we compute the rankings
    val polToTeamRanking: Map[Int, (Map[String, Int], Int)] = polToTeamRes.mapValues(groupByResult)

    polToTeamRanking
  }

  /**
    * teams are ranked as follows:
    *
    * - The list of ranks is computed for each polygon (less is better)
    * - Per-polygon ranks are summed up for each team (aggregated ranks),
    * and then the teams are sorted based on the aggregated ranks.
    *
    * - All-clean solutions are ranked higher than non-clean solutions
    * - All no-solution (no single right answer) are ranked after non-clean solutions
    *
    */
  def rankTeams(sDir: String, allTeamsResults: AllTeamsResults): Map[String, Int] = {
    val polToTeamRanking = rankForPolygons(sDir, allTeamsResults)
    val teams = BasicUtils.getTeams(sDir)

    val teamsToRanks: Seq[(String, Int, Int)] = (for {
      p <- polToTeamRanking.keys
      (sRanks, m) = polToTeamRanking(p)
      t <- teams
      tpRank = sRanks.getOrElse(t, m)
    } yield (t, p, tpRank)).toSeq
    // combine results for each team
    val teamRanksGrouped: Map[String, Seq[(String, Int, Int)]] = teamsToRanks.groupBy(_._1)
    // take the sums per team
    val teamRanksPreSummarized: Map[String, Int] = teamRanksGrouped.mapValues(s => s.map(_._3).sum)

    val mappedToRanks: Map[String, Int] = normalizeRanks(teamRanksPreSummarized, 0)

    respectCleanSolutions(mappedToRanks, allTeamsResults)
  }

  def respectCleanSolutions(ranking: Map[String, Int], allTeamsResults: AllTeamsResults): Map[String, Int] = {
    def clean(k: String) = {
      if (!allTeamsResults.isDefinedAt(k)) false
      else {
        val results = allTeamsResults(k)
        if (results.isEmpty) false
        else {
          // All solutions defined
          results.get._2.forall(_._2.isDefined)
        }
      }
    }

    def hasSmth(k: String) = {
      if (!allTeamsResults.isDefinedAt(k)) false
      else {
        val results = allTeamsResults(k)
        if (results.isEmpty) false
        else {
          // All solutions defined
          results.get._2.exists(_._2.isDefined)
        }
      }
    }

    val cleanSolutions = ranking.filterKeys(clean)
    // complement
    val greySolutions = ranking.filterKeys(k => !cleanSolutions.isDefinedAt(k) && hasSmth(k))
    val blackSolutions = ranking.filterKeys(k => !cleanSolutions.isDefinedAt(k) && !greySolutions.isDefinedAt(k))

    val normalizedClean = normalizeRanks(cleanSolutions, 0)

    // Ideally, this should be iterated
    val cleanVals = normalizedClean.values.toSeq
    val cleanPadding = if (cleanVals.isEmpty) 0 else cleanVals.max
    val normalizedGrey = normalizeRanks(greySolutions, cleanPadding)

    val greyVals = normalizedGrey.values.toSeq
    val greyPadding = if (greyVals.isEmpty) cleanPadding else greyVals.max
    val normalizedBlack = normalizeRanks(blackSolutions, greyPadding)

    val res = normalizedClean ++ normalizedGrey ++ normalizedBlack
    res
  }

  /**
    * Normalize rankings
    */
  def normalizeRanks(oldRanks: Map[String, Int], padding: Int): Map[String, Int] = {
    val ranks = oldRanks.values.toSeq.distinct.sorted
    val mappedToRanks = oldRanks.mapValues(v => (if (ranks.contains(v)) ranks.indexOf(v) + 1 else ranks.max) + padding)
    mappedToRanks
  }

  // Compute ranking for each team for a polygon based on the size of the solution
  def groupByResult(s: Seq[(String, Int)]): (Map[String, Int], Int) = {
    val grouped: Map[Int, Seq[(String)]] = s.groupBy(_._2).mapValues(z => z.map(_._1))
    // Sort with respect to solution size (smaller is better)
    val inGroups: Seq[Seq[String]] = grouped.toSeq.sortWith((a, b) => a._1 <= b._1).map(_._2)
    val reversedMap = for {
      z: Int <- inGroups.indices
      ts = inGroups(z)
      t: String <- ts
    } yield t -> (z + 1)

    (reversedMap.toMap, inGroups.size)
  }

}
