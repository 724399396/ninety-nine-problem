import org.scalatest._
import P22._

class P22Spec extends FlatSpec with Matchers {
  "range" should "success" in {
    range(4, 9) should be (List(4, 5, 6, 7, 8, 9))
  }
}
