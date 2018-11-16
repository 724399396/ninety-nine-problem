import org.scalatest._
import P07._

class P07Spec extends FlatSpec with Matchers {
  "flatten" should "flatten list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1,1,2,3,5,8))
  }
}
