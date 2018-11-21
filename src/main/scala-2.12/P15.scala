object P15 {
  def duplicateN[A](i: Int, xs: List[A]): List[A] =
    xs.flatMap(x => (1 to i) map (_ => x) )
}
