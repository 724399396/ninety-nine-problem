import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import P05._

class P05Spec extends FlatSpec with Matchers with PropertyChecks {
  "reverse" should "reverse list" in {
    reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8,5,3,2,1,1))

    forAll {
      (xs: List[Int]) => reverse(xs) should be (xs.reverse)
    }
  }
}
