import scala.util.Random

//p01
def last[A](list: List[A]): A = list match {
  case x :: Nil => x
  case _ :: xs => last(xs)
  case _ => throw new NoSuchElementException
}
last(List(1,1,2,3,5,8))
//p02
def penultimate[A](list: List[A]): A = list match {
  case x :: _ :: Nil => x
  case _ :: xs => penultimate(xs)
}

penultimate(List(1,1,2,3,5,8))

//p03
def nth[A](n: Int, list: List[A]): A =
 if(n <= 0)
   list.head
 else
   nth(n-1,list.tail)
nth(2,List(1,1,2,3,5,8))

//p04
def length[A](list: List[A]): Int = list match {
  case Nil => 0
  case x :: xs => 1 + length(xs)
}

length(List(1,1,2,3,5,8))

//p05
def reverse[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case x :: xs => reverse(xs) ++ List(x)
}
reverse(List(1,1,2,3,5,8))
//p06
def isPalindrome[A](list: List[A]): Boolean =
  reverse(list) == list

isPalindrome(List(1, 2, 3, 2, 1))
isPalindrome(List(1,2,3))

//p07
def flatten(list: List[Any]): List[Any] = {
  list.foldLeft(List[Any]())((acc,x) => x match {
    case x : List[Any] => acc ++ flatten(x)
    case _ => acc ++ List(x)
  })
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//p08
def compress[A](list: List[A]): List[A] =
  list.foldLeft(List[A]())((acc,x) => acc match {
    case Nil => List(x)
    case _ => if (x == acc.last) acc else acc ++ List(x)
  })
compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//p09
def pack[A](list: List[A]): List[List[A]] = {
  list.foldLeft(List[List[A]]())((acc,x) => acc match {
    case Nil => List(List(x))
    case _ => if (x == acc.last.head) acc.init ++ List((x :: acc.last)) else acc ++ List(List(x))
  })
}
pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

//p10
def encode[A](list: List[A]): List[(Int,A)] =
  pack(list).map(x => (x.length, x.head))

encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))


//p11
def encodeModified[A](list: List[A]): List[Any] =
  pack(list).map(x => if (x.length > 1) (x.length, x.head) else x.head)

encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

//p12
def decode(list: List[Any]): List[Any] =
  list.foldLeft(List[Any]())((acc,x) => {
    x match {
      case Tuple2(n,ele) => n match {
        case n: Int => acc ++ List.fill(n)(ele)
      }
    }
  })

decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))

//p13
def encodeDirect[A](list: List[A]): List[(Int,A)] =
  list.foldLeft(List[(Int,A)]())((acc,x) => {
    acc match {
      case Nil => List((1,x))
      case xs => {
        val ele = xs.last._2
        if (x == ele)
          xs.init ++ List((xs.last._1 + 1, x))
        else
          xs ++ List((1,x))
      }
    }
  })
encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//p14
def duplicate[A](list: List[A]): List[A] =
  list.map(x => List(x,x)).flatten

duplicate(List('a, 'b, 'c, 'c, 'd))

//p15
def duplicateN[A](repeat: Int, list: List[A]): List[A] =
  list.map(x => List.fill(repeat)(x)).flatten

duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//p16
def drop[A](nIndex: Int, list: List[A]): List[A] =
  list.grouped(nIndex).map(_.init).flatten.toList
drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

//p17
def split[A](nIndex: Int, list: List[A]): (List[A], List[A]) = {
  def help[A](n: Int, left: List[A], right: List[A]): (List[A], List[A]) =
    if (n <= 0)
      (left, right)
    else {
      val ele = right.head
      help(n - 1, left ++ List(ele), right.tail)
    }
  help(nIndex, List(), list)
}
split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

//p18
def slice[A](start: Int, end: Int, list: List[A]): List[A] =
  (for(index <- start until end) yield list(index)).toList
slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

//p19
def rotate[A](location: Int, list: List[A]): List[A] = {
  val trueLoc = if(location >= 0) location else location + list.length
  (for (index <- trueLoc until (trueLoc + list.length))
    yield list(index % list.length)).toList
}

rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

//p20
def removeAt[A](loc: Int, list: List[A]): (List[A],A) = {
  (slice(0,loc,list) ++ slice(loc+1,list.length,list), list(loc))
}

removeAt(1, List('a, 'b, 'c, 'd))

//p21
def insertAt[A](ele: A, loc: Int, list: List[A]): List[A] =
  (slice(0,loc,list) ++ List(ele) ++ slice(loc,list.length,list))

insertAt('new, 1, List('a, 'b, 'c, 'd))

//p22
def range(start: Int, end: Int): List[Int] = {
  def help(a: Int, b: Int, res: List[Int]): List[Int] = {
    if (a > b)
      res
    else {
      help(a+1,b, res ++ List(a))
    }
  }
  help(start,end,List[Int]())
}

range(4,9)

//p23
def randomSelect[A](n: Int, list: List[A]): List[A] = {
  if (n <= 0)
    Nil
  else {
    val rand = new Random()
    removeAt(rand.nextInt(list.size), list) match {
      case (rest, x) => x :: randomSelect(n - 1, rest)
    }
  }
}

randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))

//p24
def lotto(from: Int, to: Int): List[Int] = {
  val rand = new Random()
  if (rand.nextBoolean())
    (from + rand.nextInt(to - from)) :: lotto(from, to)
  else
    Nil
}

lotto(6, 49)

//p25
def randomPermute[A](list: List[A]): List[A] =
  randomSelect(list.length, list)

randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))