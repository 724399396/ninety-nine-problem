import org.scalatest._
import P16._

class P16Spec extends FlatSpec with Matchers {
  "drop" should "drop every nth element" in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    )
  }
}
