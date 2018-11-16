object P06 {
  def isPalindrome(xs: List[_]): Boolean = {
    P05.reverse(xs) == xs
  }
}
