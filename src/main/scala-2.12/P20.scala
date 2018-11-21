object P20 {
  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = {
    (xs.take(n) ++ xs.drop(n+1), xs.drop(n).head)
  }
}
