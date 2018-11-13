object P01 {
  def last[A](xs: List[A]): A = {
    xs match {
      case Nil => throw new java.util.NoSuchElementException()
      case (x :: Nil) => x
      case (_ :: left) => last(left)
    }
  }
}
