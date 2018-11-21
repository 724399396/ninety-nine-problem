object P14 {
  def duplicate[A](xs: List[A]): List[A] =
    xs.flatMap(x => List(x,x))
}
