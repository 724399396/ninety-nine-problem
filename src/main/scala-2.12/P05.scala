object P05 {
  def reverse[A](xs: List[A]): List[A] = {
    xs.foldLeft(Nil: List[A])((ys,y) => y :: ys)
  }
}
