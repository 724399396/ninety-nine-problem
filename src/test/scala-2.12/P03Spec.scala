import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import P03._

class P03Spec extends FlatSpec with Matchers {
  "nth" should "get nth element of list" in {
    nth(2, List(1,1,2,3,5,8)) should be (2)
  }
}
