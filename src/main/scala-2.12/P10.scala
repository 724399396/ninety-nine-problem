import P09._

object P10 {
  def encode[A](xs: List[A]): List[(Int, A)] = {
    pack(xs).map(y => (y.length, y.head))
  }
}
