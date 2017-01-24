object SolutionPart5 extends App {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def nodeCount: Int = 1 + children.map(_.nodeCount).sum

    def internalPathLength: Int = children.foldLeft(0)((acc,x) => acc + x.nodeCount + x.internalPathLength)

    def postorder: List[T] = children.flatMap(_.postorder) ++ List(value)

    def lispyTree: String =
      if (children.length > 0) {
        " (" + value + children.map(_.lispyTree).mkString(" ") + ")"
      } else {
        " " + value.toString
      }

    override def toString = value.toString + children.map(_.toString).mkString + "^"
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())

    def string2MTree(value: String): MTree[Char] = {
      def help(start: Int): List[String] = {
        if (start == value.length - 1) Nil
        else {
          val end = nextBound(start+1, 1)
          value.substring(start, end) :: help(end)
        }
      }

      def nextBound(pos: Int, nesting: Int): Int = {
        if (nesting == 0)
          pos
        else
          nextBound(pos+1, if (value(pos) == '^') nesting - 1 else nesting + 1)
      }

      MTree(value(0), help(1).map(string2MTree))
    }
  }

  implicit def transfer(str: String): MTree[Char] = MTree.string2MTree(str)

  // p70
  println(MTree('a', List(MTree('f'))).nodeCount)

  //p70c
  println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)

  println(transfer("afg^^c^bd^e^^^"))

  println("afg^^c^bd^e^^^".internalPathLength)
  println("afg^^c^bd^e^^^".postorder)
  println(MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree)
}
