import org.scalatest._
import P06._

class P06Spec extends FlatSpec with Matchers {
  "is palindrome" should "return true when is palindrome" in {
    isPalindrome(List(1,2,3,2,1)) should be (true)
  }
}
