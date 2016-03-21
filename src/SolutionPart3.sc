object S99Logic {
  def and(b1: Boolean, b2: Boolean): Boolean = (b1, b2) match {
    case (true, true) => true
    case _ => false
  }

  def or(b1: Boolean, b2: Boolean): Boolean = (b1, b2) match {
    case (false, false) => false
    case _ => true
  }

  def not(b: Boolean) = b match {
    case false => true
    case true => false
  }

  def equ(b1: Boolean, b2: Boolean) = or(and(b1, b2), and(not(b1), not(b2)))

  def xor(b1: Boolean, b2: Boolean) = not(equ(b1, b2))

  def nor(b1: Boolean, b2: Boolean) = not(or(b1, b2))

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

  def table2(f: (Boolean, Boolean) => Boolean) = {
    println("A     B     result")
    for {
      a <- List(true, false)
      b <- List(true, false)
    } {
      println("%-5s %-5s %-5s".format(a, b, f(a, b)))
    }
  }

  val cache = collection.mutable.Map[Int,List[String]]()

  def gray(n: Int): List[String] = {
    cache.get(n) match {
      case Some(x) => x
      case None =>
        val res = if (n == 1) {
          List("0","1")
        } else {
          gray(n - 1).flatMap { x =>
            List("0", "1").map(y => y + x)
          }
        }
        cache.put(n,res)
        res
    }
  }
  implicit def transfter(b: Boolean): S99Logic = S99Logic(b)
}

case class S99Logic(b1: Boolean) {
  import S99Logic._
  def and(b2: Boolean): Boolean = (b1, b2) match {
    case (true, true) => true
    case _ => false
  }

  def or(b2: Boolean): Boolean = (b1, b2) match {
    case (false, false) => false
    case _ => true
  }

  def equ(b2: Boolean) = (b1 and b2) or (not(b1) and not (b2))
  def xor(b2: Boolean) = not(b1 equ b2)
  def nor(b2: Boolean) = not(b1 or b2)
  def nand(b2: Boolean): Boolean = not(b1 and b2)
  def impl(b2: Boolean): Boolean = not(b1) or b2
}

import S99Logic._

table2((a: Boolean, b: Boolean) => and(a, or(a, b)))

table2((a: Boolean, b: Boolean) => a and (a or not(b)))

gray(3)

class Tree(val a: String, val weight: Int, prefix: String) {
  var pre = prefix
  def setPrefix(x: String) = {
    pre = x
  }
  def getPrefix = pre
}

case class Node(override val a: String, override val weight: Int, prefix: String, left: Tree, right:Tree) extends Tree(a,weight,prefix) {
}

case class Leaf(override val a: String, override val weight: Int, prefix: String) extends Tree(a,weight,prefix) {
}

implicit def treeOrdering: scala.math.Ordering[Tree] = new Ordering[Tree] {
  override def compare(x: Tree, y: Tree): Int = {
    val w1 = x.weight
    val w2 = y.weight
    if (w1 > w2)
      1
    else if (w1 < w2)
      -1
    else 0
  }
}

def buildTree(list: List[Tree]): List[Tree] = {
  list.sorted match {
    case x :: y :: xs =>
      val newNode = Node("", x.weight + y.weight, "", x, y)
      buildTree(newNode :: xs)
    case x => x
  }
}


def extractLeaf(tree: Tree): List[Leaf] = {
  tree match {
    case x: Leaf => List(x)
    case x: Node => extractLeaf(x.left) ++ extractLeaf(x.right)
  }
}

def generalCode(tree: Tree): Unit = {
  tree match {
    case x: Leaf =>
    case x: Node =>
      val left = x.left
      val right = x.right
      left.setPrefix(x.getPrefix + "0")
      right.setPrefix(x.getPrefix + "1")
      generalCode(left)
      generalCode(right)
  }
}

def huffman(in: List[(String,Int)]): List[(String,String)] = {
  val tree = buildTree(in.map{
    case (s,w) => Leaf(s,w,"")
  })
  generalCode(tree.head)
  extractLeaf(tree.head).map(x => (x.a, x.getPrefix))

}

huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))