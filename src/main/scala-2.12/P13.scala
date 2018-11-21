object P13 {
  def encodeDirect[A](xs: List[A]): List[(Int, A)] =
    xs.foldRight(Nil : List[(Int,A)]){(x,acc) =>
      if (acc.isEmpty)
        List((1,x))
      else if (acc.head._2 == x)
        (acc.head._1 + 1, acc.head._2) :: acc.tail
      else
        (1,x) :: acc
    }
}
