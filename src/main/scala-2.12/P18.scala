object P18 {
  def slice[A](n: Int, m: Int, xs: List[A]): List[A] =
    xs.drop(n).take(m-n)
}
