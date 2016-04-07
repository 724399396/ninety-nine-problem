def eightQueens: List[List[Int]] = {
  def isSafe(list: List[Int]): Boolean = {
    val cr = list.zipWithIndex.map {
      case (r, c) => (r,c+1)
    }
    cr.forall{ case (r,c) => cr.filter{case (r1,c1) => r1 != r && c1 != c}.forall{case (r2,c2) => Math.abs(r2 - r) != Math.abs(c2 - c)}}  &&
    (list.toSet.size == list.size)

  }

  def solve(col: Int): List[List[Int]] = {
    if (col == 1)
      (1 to 8 toList).map(List(_))
    else
      solve(col-1).flatMap{x =>
        (1 to 8).toList.map{y =>
          x ++ List(y)
        }
      }.filter(isSafe)
  }

  solve(8)
}

eightQueens