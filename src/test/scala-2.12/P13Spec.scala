import org.scalatest._
import P13._

class P13Spec extends FlatSpec with Matchers {
  "encodeDirect" should "success" in {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be ( List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
}
