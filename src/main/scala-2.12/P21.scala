object P21 {
  def insertAt[A](x: A, i: Int, xs: List[A]): List[A] = {
    xs.take(i) ++ List(x) ++ xs.drop(i)
  }
}
