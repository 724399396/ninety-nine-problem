//p01
def last[A](list: List[A]): A = list match {
  case x :: Nil => x
  case x :: xs => last(xs)
  case _ => throw new NoSuchElementException
}
last(List(1,1,2,3,5,8))
//p02
def penultimate[A](list: List[A]): A = list match {
  case x :: y :: Nil => x
  case x :: xs => penultimate(xs)
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
