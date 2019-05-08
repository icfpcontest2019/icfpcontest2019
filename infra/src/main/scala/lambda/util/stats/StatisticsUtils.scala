package lambda.util.stats

import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}

/**
  * Created by gilles on 28/01/17.
  */
trait StatisticsUtils[TeamName, AdditionalInfo] {

  // FIXED TYPES
  type ResultsToRank = IMap[TeamName, (AdditionalInfo, IMap[Int, Option[Double]])]
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
  def rankTeams(scores: ResultsToRank, precision: Int, ascOrder: Boolean): GlobalRanking = {

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