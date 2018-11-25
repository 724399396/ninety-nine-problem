import util.Random
import P20._

object P23 {
  def randomSelect[A](i: Int, xs: List[A]): List[A] = {
    val r = new Random()
    (1 to i).toList.foldRight((Nil: List[A], xs))
    {case (_, (acc, pxs)) => {
       val (lxs,x) = removeAt(r.nextInt(pxs.size), pxs)
       (x::acc, lxs)
     }
    }._1
  }
}
