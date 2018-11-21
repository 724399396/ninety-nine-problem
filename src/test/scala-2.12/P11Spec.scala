import org.scalatest._
import P11._

class P11Spec extends FlatSpec with Matchers {
  "encodeModified" should "encode run length" in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (2,'c), (2,'a), (4,'e)))
  }
}
