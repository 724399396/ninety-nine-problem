object SolutionPart5 extends App {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def nodeCount: Int = 1 + children.map(_.nodeCount).sum

    override def toString = value.toString + children.map(_.toString).mkString + "^"
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())

    def string2MTree(value: String): MTree[Char] = {
      def help(list: List[Char]): List[MTree[Char]] = list match {
        case x :: '^' :: Nil => List(MTree(x))
        case x :: xs => List(MTree(x, help(xs)))
      }

      help(value.toCharArray.toList).apply(0)
    }
  }

  implicit def transfer(str: String): MTree[Char] = MTree.string2MTree(str)

  // p70
  println(MTree('a', List(MTree('f'))).nodeCount)

  //p70c
  println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)

  println(transfer("afg^^c^bd^e^^^"))
}