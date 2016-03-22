sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T):Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[T](n: Int, v: T): List[Node[T]] = {
    if (n == 1)
      List(Node(v))
    else
      cBalanced(n-1,v).flatMap(addChild(_,v)).toSet.toList
  }

  def length[T](tree: Tree[T]): Int = tree match {
    case End => 0
    case Node(_,l,r) => 1 + length(l).max(length(r))
  }

  def addChild[T](tree: Tree[T], value: T): List[Node[T]] = {
    tree match {
      case End => List(Node(value))
      case Node(x,End,End) =>
        List(Node(x,Node(value),End), Node(x,End,Node(value)))
      case Node(x,l,End) =>
        List(Node(x,l,Node(value)))
      case Node(x,End,r) =>
        List(Node(x,Node(value),r))
      case Node (x,l,r) =>
        if (length(l) < length(r))
          addChild(l,value).map(Node(x,_,r))
        else
          addChild(r,value).map(Node(x,l,_))
    }
  }
}

Tree.cBalanced(4, 'a')