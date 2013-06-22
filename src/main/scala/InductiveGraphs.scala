object Graphs extends App {

  type Node = Int
  type Adj[+B] = Seq[(B, Node)]
  case class Context[+A, +B](incoming: Adj[B], node: Node, value: A, outgoing: Adj[B]) {
    override def toString = s"([${incoming mkString " "}]→ $node($value) →[${outgoing mkString " "}])"
  }

  sealed trait Graph {
    def &:[A, B](context: Context[A, B]) = Graphs.&:(context, this)
  }
  case object Empty extends Graph
  final case class &:[A, B](left: Context[A, B], right: Graph) extends Graph {
    override def toString = left + " &: " + right
  }

}
