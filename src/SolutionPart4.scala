object SolutionPart4 extends App {

  sealed abstract class Tree[+T] {
    def isSymmetric: Boolean

    def addValue[U >: T <% Ordered[U]](value: U): Tree[U]

    def height: Int

    def length: Int

    def flip: Tree[T]

    def isBalance: Boolean

    def leafCount: Int

    def leafList: List[T]

    def internalList: List[T]

    def atLevel(level: Int): List[T]

    def layoutBinaryTree: Tree[T]

    def layoutBinaryTree2: Tree[T]
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    override def isSymmetric = true

    override def addValue[U >: Nothing <% Ordered[U]](value: U): Tree[U] = Node(value)

    override def height = 0

    override def length = 0

    override def flip = End

    override def isBalance = true

    override def leafCount: Int = 0

    override def leafList: List[Nothing] = Nil

    override def internalList: List[Nothing] = Nil

    override def atLevel(level: Int): List[Nothing] = Nil

    override def layoutBinaryTree: Tree[Nothing] = End

    override def layoutBinaryTree2: Tree[Nothing] = End
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    override def isSymmetric = this match {
      case Node(_, End, End) => true
      case Node(_, l, End) => false
      case Node(_, End, r) => false
      case Node(_,l,r) => isMirrorOf(l,r)
    }

    override def flip = this match {
      case Node(x,l,r) => Node(x,r.flip,l.flip)
    }

    override def height = this match {
      case Node(_, End, End) => 1
      case Node(_, l, r) => 1 + Math.max(l.height, r.height)
    }

    override def length = this match {
      case Node(_, End, End) => 1
      case Node(x, l, r) =>
        1 + l.length + r.length
    }

    override def addValue[U >: T <% Ordered[U]](value: U): Tree[U] = this match {
      case Node(x, l, r) =>
        if (value < x)
          Node(x, l.addValue(value), r)
        else
          Node(x, l, r.addValue(value))
    }

    override def isBalance: Boolean = this match {
      case Node(x,l,r) => Math.abs(l.height - r.height) <= 1
    }

    override def leafCount: Int = this match {
      case Node(x,End,End) => 1
      case Node(x,l,r) => l.leafCount + r.leafCount
    }

    override def leafList: List[T] = this match {
      case Node(x,End,End) => List(x)
      case Node(x,l,r) => l.leafList ++ r.leafList
    }

    override def internalList: List[T] = this match {
      case Node(x,End,End) => List()
      case Node(x,l,r) => x :: (l.internalList ++ r.internalList)
    }

    override def atLevel(level: Int): List[T] = {
      if (level == 1)
        this match {
          case Node(x,_,_) => List(x)
        }
      else
        this match {
          case Node(_,l,r) => l.atLevel(level - 1) ++ r.atLevel(level - 1)
        }
    }

    override def layoutBinaryTree: Tree[T] = {
      def help(tree: Tree[T], lSize: Int, isRight: Boolean, level: Int): Tree[T] = {
        tree match {
          case node@Node(x, l, r) =>
            val xLoc = if(isRight) lSize + l.length + 1 else lSize - r.length - 1
            PositionedNode(x, help(l,xLoc, false, level+1), help(r,xLoc, true,level+1), xLoc, level)
          case End => End
        }
      }
      help(this,this.length + 1,false,1)
    }

    override def layoutBinaryTree2: Tree[T] = {
      val maxLevel = this.height
      def help(tree: Tree[T], lSize: Int, isRight: Boolean, level: Int): Tree[T] = {
        tree match {
          case node@Node(x, l, r) =>
            if (x == 'a') {
              println('a')
            }
            val common = math.pow(2,maxLevel - level).toInt
            val xLoc = if(isRight) lSize + common else lSize - common
            PositionedNode(x, help(l,xLoc, false, level+1), help(r,xLoc, true,level+1), xLoc, level)
          case End => End
        }
      }
      help(this,Math.pow(2,height).toInt-1,false,1)
    }
  }

  class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  object PositionedNode {
    def apply[T](value: T, l: Tree[T], r: Tree[T], x: Int, y: Int) = new PositionedNode(value,l,r,x,y)
  }

  def isMirrorOf[T](t1: Tree[T], t2: Tree[T]): Boolean = {
    (t1,t2) match {
      case (End,End) => true
      case (End,_) => false
      case (_,End) => false
      case (Node(_,End,End),Node(_,End,End)) => true
      case (Node(_,l,End),Node(_,End,r)) => isMirrorOf(l,r)
      case (Node(_,End,r),Node(_,l,End)) => isMirrorOf(r,l)
      case (Node(_,l,r), Node(_,l1,r1)) => isMirrorOf(l,r1) && isMirrorOf(r,l1)
    }
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    // p55
    def cBalanced[T](n: Int, v: T): List[Tree[T]] = {
      if (n == 1)
        List(Node(v))
      else
        cBalanced(n - 1, v).flatMap(addChild(_, v)).toSet.toList
    }

    def height[T](tree: Tree[T]): Int = tree match {
      case End => 0
      case Node(_, l, r) => 1 + height(l).max(height(r))
    }

    def addChild[T](tree: Tree[T], value: T): List[Node[T]] = {
      tree match {
        case End => List(Node(value))
        case Node(x, End, End) =>
          List(Node(x, Node(value), End), Node(x, End, Node(value)))
        case Node(x, l, End) =>
          List(Node(x, l, Node(value)))
        case Node(x, End, r) =>
          List(Node(x, Node(value), r))
        case Node(x, l, r) =>
          if (height(l) < height(r))
            addChild(l, value).map(Node(x, _, r))
          else
            addChild(r, value).map(Node(x, l, _))
      }
    }

    def fromList[T <% Ordered[T]](list: List[T]): Tree[T] = {
      var res: Tree[T] = End
      for(x <- list)
        res = res.addValue(x)
      res
    }

    def symmetricBalancedTrees[T](n: Int, value: T): List[Tree[T]] = {
      cBalanced(n,value).filter(_.isSymmetric)
    }

    def hbalTrees[T](n: Int, value: T): List[Tree[T]] = {
      (n to (Math.pow(2,n) - 1).toInt).flatMap(x => {
        cBalanced(x, value)
      }).filter(_.height == n).filter(_.isBalance).toSet.toList
    }

    def completeBinaryTree[T](n: Int, value: T): Tree[T] = {
      def help(parent: Int): Tree[T] = {
        if (parent * 2 + 1 <= n)
          Node(value,help(parent*2), help(parent*2+1))
        else if (parent * 2 <= n)
          Node(value,help(parent*2), End)
        else
          Node(value,End,End)
      }
      help(1)
    }
  }

  def minHbalNodes(height: Int): Int = {
    Tree.hbalTrees(height,"x").map(_.length).min
  }

  def maxHbalHeight(node: Int): Int = {
    Tree.cBalanced(node,"x").filter(_.isBalance).map(_.height).max
  }

  println(Tree.cBalanced(4, 'a'))

  println(Node('a', Node('b'), Node('c')).isSymmetric)

  println(End.addValue(2).addValue(3).addValue(0))

  println(Tree.fromList(List(3, 2, 5, 7, 1)))

  println(Tree.fromList(List(3,2,5,7,1)).flip)

  println(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)

  println(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric)

  println(Tree.symmetricBalancedTrees(5, "x"))

  println(Tree.hbalTrees(3, "x"))

  println(minHbalNodes(3))
  println(maxHbalHeight(4))

  println(Node('x', Node('x'), End).leafCount)

  println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList)

  println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)
  println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))
  println(Tree.completeBinaryTree(6, "x"))
  println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree)
  println(Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q')).layoutBinaryTree)
  println(Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q')).layoutBinaryTree2)
}