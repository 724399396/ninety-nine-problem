import org.scalatest._
import P08._

class P08Spec extends FlatSpec with Matchers {
  "compress" should "remove continue duplicate" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }
}
