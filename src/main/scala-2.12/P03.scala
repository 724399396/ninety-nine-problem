object P03 {
  def nth[A](n: Int, xs: List[A]): A = {
    if (n <= 0)
      xs.head
    else
      nth(n-1, xs.tail)
  }
}
