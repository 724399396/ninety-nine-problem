import org.scalatest._
import P17._

class P17Spec extends FlatSpec with Matchers {
  "split" should "split every nth element" in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (
      (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
  }
}
