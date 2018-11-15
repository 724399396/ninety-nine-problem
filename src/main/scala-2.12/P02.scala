object P02 {
  def penultimate[A](xs: List[A]): A = {
    xs match {
      case Nil => throw new java.util.NoSuchElementException()
      case (_ :: Nil) => throw new java.util.NoSuchElementException()
      case (x :: _ :: Nil) => x
      case (_ :: left) => penultimate(left)
    }
  }
}
