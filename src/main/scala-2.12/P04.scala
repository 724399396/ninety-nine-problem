object P04 {
  def length(xs: List[_]): Int = {
    xs match {
      case Nil => 0
      case _ :: left => 1 + length(left)
    }
  }
}
