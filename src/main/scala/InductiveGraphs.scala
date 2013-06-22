object Graphs extends App {

  type Node = Int
  type Adj[+B] = Seq[(B, Node)]
  case class Context[+A, +B](incoming: Adj[B], node: Node, value: A, outgoing: Adj[B]) {
    override def toString = s"([${incoming mkString " "}]→ $node($value) →[${outgoing mkString " "}])"
  }

  sealed trait Graph {
    def &:[A, B](context: Context[A, B]) = Graphs.&:(context, this)

    def isEmpty: Boolean

    def gmap[A, B, C, D](f: Context[A, B] ⇒ Context[C, D]): Graph = this match {
      case Empty ⇒ Empty
      case &:(left: Context[A, B], right) ⇒ f(left) &: right.gmap(f)
    }

  }
  case object Empty extends Graph {
    def isEmpty = true
  }
  final case class &:[A, B](left: Context[A, B], right: Graph) extends Graph {
    def isEmpty = false
    override def toString = left + " &: " + right
  }

}
