import org.scalatest._
import P20._

class P20Spec extends FlatSpec with Matchers {
  "removeAt" should "remove Nth element" in {
    removeAt(1, List('a, 'b, 'c, 'd)) should be (
      (List('a, 'c, 'd),'b)
    )
  }
}
