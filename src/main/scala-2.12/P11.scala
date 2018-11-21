import P10._

object P11 {
  def encodeModified[A](xs: List[A]): List[(Int, A)] = {
    encode(xs).filter(_._1 > 1)
  }
}
