object P07 {
  def flatten(xs: List[Any]): List[Any] = {
    xs.foldRight(Nil: List[Any])((x,xs) => {
                                   x match {
                                     case ys : List[Any] => flatten(ys)
                                     case y => List(y)
                                   }
                                 } ++ xs)
  }
}
