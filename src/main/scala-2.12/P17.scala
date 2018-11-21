object P17 {
  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    (xs.take(n), xs.drop(n))
  }
}
