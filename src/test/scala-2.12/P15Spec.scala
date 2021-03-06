import org.scalatest._
import P15._

class P15Spec extends FlatSpec with Matchers {
  "duplicateN" should "duplicate every element n times" in {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be (
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
}
