object P08 {
  def compress[A](xs: List[A]): List[A] = {
    xs.foldRight((Nil: List[A], None: Option[A])){
      case (x, (acc, preX)) =>
        val equal = preX.map(_ == x).getOrElse(false)
        if (equal)
          (acc, preX)
        else
          (x::acc, Some(x))
    }._1
  }
}
