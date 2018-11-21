import org.scalatest._
import P18._

class P18Spec extends FlatSpec with Matchers {
  "slice" should "slice from n to m" in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    (List('d, 'e, 'f, 'g))
  }
}
