object P16 {
  def drop[A](i: Int, xs: List[A]): List[A] =
    xs.zipWithIndex.filter{
      case (_, index) => (index+1) % i != 0
    }.map(_._1)
}
