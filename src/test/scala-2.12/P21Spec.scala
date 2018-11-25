import org.scalatest._
import P21._

class P21Spec extends FlatSpec with Matchers {
  "insertAt" should "success" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
  }
}
