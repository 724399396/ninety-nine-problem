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
      def help(node: Node, path: List[T]): List[List[T]] = {
        if (node.value == v2)
          List(path)
        else {
          nodes(node.value).adj.map(edgeTarget(_, node).get).filter(n => !path.contains(n.value)).flatMap{x => help(x, x.value :: path)}
        }
      }
      help(nodes(v1), List(v1)).map(_.reverse)
    }

    def findCycles(v:  T): List[List[T]] = {
      val n = nodes(v)
      n.adj.map(edgeTarget(_, n).get.value).flatMap(findPaths(_, v)).map(v :: _).filter(_.lengthCompare(3) > 0)
    }

    def spanningTrees: List[Graph[T, U]] = {
      def subEdges(edge: List[Edge]): List[List[Edge]] = edge match {
        case Nil => List()
        case x :: Nil => List(List(x), List())
        case x :: xs =>
          val sub = subEdges(xs)
          sub ++ sub.map(x :: _)
      }
      def connect(edge: List[Edge]) = {
        var connectNode: Set[Node] = Set()
        var duplicate = false
        for (e <- edge) {
          if (connectNode.contains(e.n1) && connectNode.contains(e.n2))
            duplicate = true
          connectNode += e.n1
          connectNode += e.n2
        }
        connectNode.map(_.value) == nodes.keys && !duplicate
      }
      subEdges(edges).filter(connect).map { x =>
        val res = new Graph[T,U]()
        x.foreach { e =>
          res.addNode(e.n1.value)
          res.addNode(e.n2.value)
          res.addEdge(e.n1.value, e.n2.value, e.value)
        }
        res
      }
    }

    def minimalSpanningTree: Graph[T, U] = {
      spanningTrees.minBy{ graph =>
        graph.edges.map(_.value.asInstanceOf[Int]).sum
      }
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

    override def toString = edges.map(x => x.n1.value + "-" + x.n2.value + (
      if (x.value != ()) "/" + x.value else ""
      )).mkString("[",",","]")
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
      str.drop(1).dropRight(1).split(",").foreach { x =>
        if (x.contains(">")) {
          x.split(">") match {
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
        } else {
          x.split("-") match {
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
      }
      res
    }
  };

  /* println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm)

  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm)

  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q"))

  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k"))

  println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f")) */

  println(Graph.fromString("[a-b, b-c, a-c]").spanningTrees)

  println(Digraph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree)
}