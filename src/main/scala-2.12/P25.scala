import P23._

object P25 {
  def randomPermute[A](xs: List[A]): List[A] = {
    randomSelect(xs.size, xs)
  }
}
