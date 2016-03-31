/**
  * @author liww.li
  * @date 2016-03-30 15:28
  */
object SolutionPart6 extends App {
  abstract class GraphBase[T, U] {
    case class Edge(n1: Node, n2: Node, value: U) {
      def toTuple = (n1.value, n2.value, value)
    }

    case class Node(value: T) {
      var adj: List[Edge] = Nil
      def neighbors: List[Node] = adj.map(edgeTarget(_,this).get)
    }

    var nodes: Map[T, Node] = Map()
    var edges: List[Edge] = Nil

    def edgeTarget(e: Edge, n: Node): Option[Node]

    override def equals(o: Any) = o match {
      case g: GraphBase[T,U] => (nodes.keys.toSet -- g.nodes.keys.toSet == Nil &&
                                 edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple).toSet == Nil)
      case _ => false
    }

    def addNode(value: T) = {
      val n = new Node(value)
      nodes = Map(value -> n) ++ nodes
    }

    def findPaths(v1: T, v2: T): List[List[T]] = {
      (nodes(v1).neighbors.map(_.value).filter(_ == v2) headOption match {
        case Some(x) => List(v1,x)
        case None => Nil
      })::
        nodes(v1).neighbors.map(_.value).filter(_ != v2).flatMap(this.findPaths(_,v2)).filter(_.length > 0).map(v1 :: _)
    }
  }

  class Graph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Graph[T, U] => super.equals(o)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node) = {
      if (e.n1 == n) Some(e.n2)
      else if (e.n2 == n) Some(e.n1)
      else None
    }

    def addEdge(n1: T, n2: T, value: U) = {
      val e = new Edge(new Node(n1), new Node(n2), value)
      edges = e :: edges
      nodes(n1).adj = e :: nodes(n1).adj
      nodes(n2).adj = e :: nodes(n2).adj
    }

    def toTermForm: (List[T], List[(T,T,U)]) = {
      (nodes.keys.toList, edges.map(_.toTuple))
    }
  }

  object Graph {
    def fromString(str: String): Graph[String, Unit] = {
      val res = new Graph[String, Unit]
      str.drop(1).dropRight(1).split(",").foreach {
        _.split("-") match {
          case Array(n1, n2) =>
            res.addNode(n1.trim)
            res.addNode(n2.trim)
            res.addEdge(n1.trim, n2.trim, ())
          case Array(n) =>
            res.addNode(n.trim)
        }
      }
      res
    }
  }

  class DiGraph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Graph[T, U] => super.equals(o)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node) = {
      if (e.n1 == n) Some(e.n2)
      else None
    }

    def addArc(source: T, dest: T, value: U) = {
      val e = new Edge(new Node(source), new Node(dest), value)
      edges = e :: edges
      nodes(source).adj = e :: nodes(source).adj
    }

    def toTermForm: (List[T], List[(T,T,U)]) = {
      (nodes.keys.toList, edges.map(_.toTuple))
    }

    def toAdjacentForm: List[(T,List[(T,U)])] = {
      nodes.values.toList.map{node =>
        (node.value, node.adj.map(x => (x.n2.value, x.value)))
      }
    }
  }

  object Digraph {
    def fromStringLabel(str: String): DiGraph[String, Int] = {
      val res = new DiGraph[String,Int]()
      str.drop(1).dropRight(1).split(",").foreach {
        _.split(">") match {
          case Array(n1, y) =>
            y.split("/") match {
              case Array(n2, w) => {
                res.addNode(n1.trim)
                res.addNode(n2.trim)
                res.addArc(n1.trim, n2.trim, w.toInt)
              }
            }
          case Array(n) =>
            res.addNode(n.trim)
        }
      }
      res
    }
  }

  println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm)

  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm)

  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q"))

  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k"))

  println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f"))
}
