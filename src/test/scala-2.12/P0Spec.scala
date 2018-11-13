import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import P01._

class P01Spec extends FlatSpec with Matchers with PropertyChecks {
  "last" should "get list last element" in {
    forAll (arbitrary[List[Int]].suchThat(!_.isEmpty)) {(xs: List[Int]) => xs.last should be (last(xs))}
  }
}
