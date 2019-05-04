package lambda.chain
import lambda.contest.Booster
import lambda.contest.Booster.Booster
import org.scalatest.{FlatSpec, Matchers}

class WalletTests extends FlatSpec with Matchers {
  private def testAddress(sk: SecretKey, expected: String) =
    s"The wallet address" should
      s"should be generated correctly from the secret key $sk." in {
      assertResult(expected) {
        new Wallet(sk).address
      }
    }

  private def createsValidTx(w: Wallet, sk: SecretKey, inputCoins: Long, outputBoosters: Map[Booster, Long],
                             msg: String) = {
    s"The wallet with secret $sk" should
      s"creates transaction ($inputCoins, $outputBoosters, $msg)  for which the HMAC check passes." in {
      val tx = w.createTransaction(inputCoins, outputBoosters, msg)
      assert(tx.validMac(sk))
    }
  }

  private def transactionNotMalleable(w: Wallet, sk: SecretKey, inputCoins: Long, outputBoosters: Map[Booster, Long],
                             msg: String) = {
    s"The wallet with secret $sk" should
      s"creates transaction ($inputCoins, $outputBoosters, $msg) which is not malleable." in {
      val tx = w.createTransaction(inputCoins, outputBoosters, msg)
      val forged = tx.copy(inputCoins=inputCoins + 1)
      assert(forged.validMac(sk) == false)
    }
  }

  testAddress("123", "0c26878aec02f0129cde34613dcd1a0c05e008c94735fc95c4a3ae3a86030675")
  testAddress("super_secret", "5935043033ea53e3cbf7ed5c3d16aca82ab571def453add6bce95c6b72e5f0ed")

  val sk1 = "123"
  val wallet1 = new Wallet(sk1)

  createsValidTx(wallet1, sk1, 1000, Map(Booster.BatteriesBooster -> 1), "")
  createsValidTx(wallet1, sk1, 23, Map(Booster.BatteriesBooster -> 4, Booster.CoffeeBooster -> 35), "33")
  transactionNotMalleable(wallet1, sk1, 1000, Map(Booster.BatteriesBooster -> 1), "")
}
