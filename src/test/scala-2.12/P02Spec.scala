import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import P02._

class P02Spec extends FlatSpec with Matchers with PropertyChecks {
  "penultimate" should "get list last previous one element" in {
    penultimate(List(1,1,3,5,8)) should be (5)

    forAll(arbitrary[List[Int]].suchThat(_.size >= 2)) {(xs: List[Int]) => xs.reverse.tail.head should be (penultimate(xs))}
  }
}
