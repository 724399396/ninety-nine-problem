import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

class P04Spec extends FlatSpec with Matchers with PropertyChecks {
  "length" should "get list length" in {
    P04.length(List(1, 1, 2, 3, 5, 8)) should be (6)

    forAll {
      (xs: List[Int]) => P04.length(xs) should be (xs.size)
    }
  }
}
