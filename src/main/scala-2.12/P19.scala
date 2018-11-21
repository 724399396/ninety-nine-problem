object P19 {
  def rotate[A](n: Int, xs: List[A]): List[A] = {
    if (n > 0)
      xs.drop(n) ++ xs.take(n)
    else
      xs.reverse.take(-n).reverse ++ xs.reverse.drop(-n).reverse
  }
}
