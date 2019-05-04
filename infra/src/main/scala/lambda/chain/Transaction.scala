package lambda.chain

import com.roundeights.hasher.Algo
import lambda.contest.Booster.Booster
import lambda.contest.ContestConstants.Boosters

import scala.collection.mutable

// Always construct these via Wallet.createTransaction!
case class Transaction(inputAddress: WalletAddress, seqNum: Long, inputCoins: Long,
                       outputBoosters: Map[Booster, Long], msg: String, sig: String) {


  private def canonicalMap(ob: Map[Booster, Long]): String = {
    val cm = mutable.Map[Booster, Long]() ++= ob
    cm.retain((k, v) => Boosters.contains(k))
    for (b <- Boosters) {
      cm.getOrElseUpdate(b, 0)
    }
    cm.toSeq.sortBy(_._1).mkString("|")
  }

  def canonicalString(): String = {
    s"$inputAddress|$seqNum|$msg|$inputCoins|" + canonicalMap(outputBoosters)
  }

  def validMac(secretKey: SecretKey): Boolean = {
    hmac(secretKey, canonicalString) hash= sig
}

  private def hmac(secretKey: SecretKey, msg: String) = {
    Algo.hmac(secretKey).sha256(msg)
  }
}
