object P12 {
  def decode[A](xs: List[(Int, A)]): List[A] =
    xs.foldRight(Nil: List[A])((x,acc) => (1 to x._1).map(_ => x._2).toList ++ acc)
}
