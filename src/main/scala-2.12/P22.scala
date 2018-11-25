object P22 {
  def range(from: Int, to: Int) = {
    def step(i: Int, acc: List[Int]): List[Int] = {
      if (i < from) acc
      else step(i-1, i :: acc)
    }
    step(to, Nil)
  }
}
