import org.scalatest._
import P14._

class P14Spec extends FlatSpec with Matchers {
  "duplicate" should "duplicate every element" in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be (
      List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }
}
