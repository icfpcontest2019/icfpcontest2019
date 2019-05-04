package lambda.chain
import org.scalatest.{FlatSpec, Matchers}
import spray.json._
import lambda.chain.LambdaJsonProtocol._
import lambda.contest.Booster

class TransactionTests extends FlatSpec with Matchers {
  private def testSerialiseDeserialise(tx: Transaction) =
    s"A transaction" should
      s"should serialise correctly to JSON." in {
      assertResult(tx) {
        tx.toJson.convertTo[Transaction]
      }
    }

  private def testCanonical(tx: Transaction, expected: String) = {
    s"A transaction" should
      s"should have a canonical form." in {
      assertResult(expected) {
        tx.canonicalString
      }
    }
  }

  val tx1 = Transaction(
    "0c26878aec02f0129cde34613dcd1a0c05e008c94735fc95c4a3ae3a86030675",
    1,
    1000,
    Map(Booster.BatteriesBooster -> 1, Booster.CoffeeBooster -> 20),
    "user provided messages",
    "fake signature"
  )

  testSerialiseDeserialise(tx1)
  testCanonical(tx1, "0c26878aec02f0129cde34613dcd1a0c05e008c94735fc95c4a3ae3a86030675|1|user provided messages|1000|(BatteriesBooster,1)|(CoffeeBooster,20)|(DrillBooster,0)|(TeleportBooster,0)|(CallWatchmanBooster,0)|(CallPoint,0)")
}
