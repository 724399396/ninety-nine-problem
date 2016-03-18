case class S99Int(n: Int) {
  import S99Int._
  //p31
  def isPrime: Boolean =
    (n > 1) && (primes takeWhile { _ <= Math.sqrt(n) } forall { n % _ != 0 })

  //p33
  def isCoprimeTo(other: S99Int): Boolean =
    gcd(n, other.n) == 1

  //p34
  def totient: Int =
    (1 to n).toList.filter(isCoprimeTo(_)).size

  //p35
  def primeFactors: List[Int] =
    if (isPrime)
      List(n)
    else {
      primes.find(x => n / x * x == n) match {
        case Some(x) => x :: (n/x).primeFactors
        case None => Nil
      }
    }

  //p36
  def primeFactorMultiplicity: List[(Int,Int)] =
    primeFactors.groupBy(identity).toList.map(group2KeyAndTime)

  def group2KeyAndTime(t: (Int,List[Int])):(Int,Int) =
    (t._2.head, t._2.length)
}

case object S99Int{
  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
}

implicit def transfer(n: Int): S99Int = S99Int(n)

17.isPrime

//p32
def gcd(a: Int, b: Int): Int = {
  if (b == 0)
    a
  else
    gcd(b, a % b)
}

gcd(36, 63)

35.isCoprimeTo(64)

10.totient

315.primeFactors

315.primeFactorMultiplicity