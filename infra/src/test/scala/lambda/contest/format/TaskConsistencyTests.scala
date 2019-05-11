package lambda.contest.format

import lambda.contest.ContestConstants
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class TaskConsistencyTests extends FlatSpec with Matchers {

  "All used letters" should "be distinct" in {
    val letters = ContestConstants.CONTEST_LETTERS.sorted
    val d = letters.distinct
    assertResult(letters)(d)
  }

}
