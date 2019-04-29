package lambda.util.stats

import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}

/**
  * Created by gilles on 28/01/17.
  */
trait StatisticsUtils[AdditionalInfo] {

  // TEAMNAME AND TEAMINFO TYPES CAN BE CHANGED TO SOMETHING ELSE IF NEEDED
  type TeamName = String

  // FIXED TYPES
  type resultsToRank = IMap[TeamName, (AdditionalInfo, IMap[Int, Option[Double]])]
  type GlobalRanking = IMap[Int, Seq[TeamName]] // Map from Global Rank to Team(s)


  def roundToPrecision(d: Double, precision: Int): Double = {
    BigDecimal(d).setScale(precision, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  /**
    * Returns a map from Global Rank to Team(s) info
    *
    * @param scores contains a mapping from teams to their results and meta info. Lowest score is best (unless None then it's worst)
    * @return
    */
  def rankTeams(scores: resultsToRank, precision: Int, ascOrder: Boolean): GlobalRanking = {

    var points: Map[TeamName, Int] = Map() // Map from teamname to total of points

    for (team <- scores) {
      val teamResultMap: IMap[Int, Option[Double]] = team._2._2
      val resultSeq = teamResultMap.toSeq.sortBy(_._1).map(_._2)
      val teamPoints = mutable.Buffer.fill(resultSeq.size)(0)
      for ((t, p) <- resultSeq.zipWithIndex) {
        var betterTeams = 0
        t match {
          case None => betterTeams = scores.keys.size
          case Some(z) =>
            scores.foreach {
              s => {
                s._2._2.get(p+1) match {
                  case Some(Some(t2)) =>
                    val tRound = roundToPrecision(z, precision)
                    val t2Round = roundToPrecision(t2, precision)
                    val eps = Math.pow(10, -precision)
                    if (((tRound - t2Round) < eps) && ascOrder) {
                      betterTeams = betterTeams + 1
                    }
                    if (((tRound - t2Round) > eps) && !ascOrder) {
                      betterTeams = betterTeams + 1
                    }
                  case _ =>
                }
              }
            }
        }
        teamPoints(p) = scores.keys.size - betterTeams
      }
      val totalPoints = teamPoints.sum
      points += (team._1 -> totalPoints)
    }
    //    points.foreach(println(_))

    var ranks = Map[Int, Seq[TeamName]]()
    var currentRank = 1

    while (points.keys.nonEmpty) {

      var highestScore = 0
      var tiedTeams = ArrayBuffer[TeamName]()
      for (teamPoints <- points) {

        if (teamPoints._2 == highestScore) {
          tiedTeams += teamPoints._1
        }
        if (teamPoints._2 > highestScore) {
          tiedTeams.clear()
          tiedTeams += teamPoints._1
          highestScore = teamPoints._2
        }
      }

      ranks += (currentRank -> tiedTeams)
      tiedTeams.foreach(points.remove(_))
      currentRank = currentRank + 1
    }
    ranks.toMap
  }

}

/**
  * [Ilya] Sorry, Gilles, but I don't have time to refactor the imperative code below.
  */

/*
object StatisticsUtil extends StatisticsUtils[(String, String)] {
  /**
    * Main class just to test ranking on a hardcoded input
    */
  def main(args: Array[String]): Unit = {

    val numberOfProblems = 30
    val precision = 0.000001f

    // initialise a hardcoded array for testing purposes
    val teaminfos = Seq(("team1", "10/20/17"), ("team2", "10/20/17"), ("team3", "10/20/17"), ("team4", "10/20/17"))

    // generate random scores
    var teamscores = Buffer.fill(4)(Buffer.fill(numberOfProblems)(Option(0.0f))) //--> teamscores(teamIndex)(problemIndex)
    for (i <- teamscores.indices) {
      var temp = Buffer.fill(numberOfProblems)(Option(0.0f))
      for (j <- temp.indices) {
        temp(j) = Some((math.random * 99 + 1).toFloat)
      }
      teamscores(i) = temp
    }

    //then randomly change some to None
    val rnd = new scala.util.Random
    for (i <- 1 to 10)
      teamscores(rnd.nextInt(4))(rnd.nextInt(numberOfProblems)) = None


    val teamNames = Seq("alpha", "bravo", "charlie", "delta")

    println(teamscores)

    var toRank: resultsToRank = mutable.Map()
    for (i <- teamNames.indices)
      toRank += (teamNames(i) -> (teaminfos(i), teamscores(i)))
    val finalRanking = rankTeams(toRank, precision)

    for (i <- 1 to teaminfos.length) {
      if (finalRanking.keySet.contains(i))
        println("Rank " + i + ": " + finalRanking(i))
    }

    // plot the scores (plot all teamscores(i))
  }

}
*/


