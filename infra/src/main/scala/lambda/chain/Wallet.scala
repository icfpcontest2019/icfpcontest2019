package lambda.chain

import com.roundeights.hasher.Algo
import lambda.contest.Booster.Booster

/*
  The Wallet is responsible for:
    - keeping the team's secret
    - generating the team's wallet address
    - "signing" (HMAC) transactions and puzzle submissions
 */

object WalletConstants {
  val ADDRESS_SEED = "addr_of"
}

class Wallet(secretKey: SecretKey) {
  var txCount = 0
  val address : WalletAddress = hmac(WalletConstants.ADDRESS_SEED).hex
  private def hmac(msg: String) = {
    Algo.hmac(secretKey).sha256(msg)
  }

  def createTransaction(inputCoins: Long, outputBoosters: Map[Booster, Long], msg: String): Transaction = {
    val unsignedTx = Transaction(address, txCount + 1, inputCoins, outputBoosters, msg, "")
    val signature = hmac(unsignedTx.canonicalString).hex
    txCount += 1
    unsignedTx.copy(sig=signature)
  }
}
