object P24 {
  def lotto(n: Int, to: Int): List[Int] = {
    val r = new util.Random()
    (1 to n).toList(_ => r.nextInt(to + 1))
  }
}
