object P09 {
  def pack[A](xs: List[A]): List[List[A]] = {
    val (acc, left) = xs.foldRight((Nil: List[List[A]], Nil: List[A])){
      case (x, (acc, preXs)) =>
        val equal = preXs.exists(_ == x)
        if (equal || preXs.isEmpty)
          (acc, x :: preXs)
        else
          (preXs :: acc, List(x))
    }
    if (left.isEmpty)
      acc
    else
      left :: acc
  }
}
