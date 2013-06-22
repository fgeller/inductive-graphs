object Graphs extends App {

  type Node = Int
  type Adj[+B] = Seq[(B, Node)]
  case class Context[+A, +B](incoming: Adj[B], node: Node, value: A, outgoing: Adj[B]) {
    override def toString = s"([${incoming mkString " "}]→ $node($value) →[${outgoing mkString " "}])"
  }

  sealed trait Graph[+A, +B] {
    def &:[C >: A, D >: B](context: Context[C, D]) = Graphs.&:(context, this)

    def isEmpty: Boolean

    def gmap[C >: A, D >: B](f: Context[A, B] ⇒ Context[C, D]): Graph[C, D] = this match {
      case Empty                          ⇒ Empty
      case &:(left: Context[A, B], right) ⇒ f(left) &: right.gmap(f)
    }

    def grev: Graph[A, B] = gmap { left: Context[A, B] ⇒
      Context(left.outgoing, left.node, left.value, left.incoming)
    }

    def ufold[C](memo: C)(f: (C, Context[A, B]) ⇒ C): C = this match {
      case Empty                          ⇒ memo
      case &:(left: Context[A, B], right) ⇒ right.ufold(f(memo, left))(f)
    }

  }
  case object Empty extends Graph[Nothing, Nothing] {
    def isEmpty = true
  }
  final case class &:[A, B](left: Context[A, B], right: Graph[A, B]) extends Graph[A, B] {
    def isEmpty = false
    override def toString = left + " &: " + right
  }

}
