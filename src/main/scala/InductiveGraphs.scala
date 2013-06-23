object Graphs extends App {

  type Node = Int
  type Adj[+B] = Seq[(B, Node)]
  case class Context[+A, +B](incoming: Adj[B], node: Node, value: A, outgoing: Adj[B]) {
    def suc = outgoing map (_._2)
    override def toString = s"([${incoming mkString " "}]→ $node($value) →[${outgoing mkString " "}])"
  }

  sealed trait Graph[+A, +B] {
    def &:[C >: A, D >: B](context: Context[C, D]) = Graphs.&:(context, this)

    def isEmpty: Boolean = this match {
      case Empty ⇒ true
      case _     ⇒ false
    }

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

    def nodes: Seq[Node] = this match {
      case Empty                          ⇒ Seq()
      case &:(left: Context[A, B], right) ⇒ right.ufold(List(left.node)) { (memo, ctx) ⇒ ctx.node :: memo }
    }

    def undir: Graph[A, B] = gmap { ctx ⇒
      Context(ctx.incoming ++ ctx.outgoing, ctx.node, ctx.value, ctx.incoming ++ ctx.outgoing)
    }

    def degree(node: Node) = SearchNode(this, node) match {
      case FindNode(Context(in, _, _, out), _) ⇒ Some(in.size + out.size)
      case _                                   ⇒ None
    }

    def delete(node: Node) = SearchNode(this, node) match {
      case FindNode(_, restGraph) ⇒ restGraph
      case _                      ⇒ this
    }

    def gsuc(node: Node) = SearchNode(this, node) match {
      case FindNode(Context(_, _, _, out), _) ⇒ out map (_._2)
      case _                                  ⇒ Seq()
    }
  }

  object Graph {
    def empty[A, B]: Graph[A, B] = Empty
  }

  case object Empty extends Graph[Nothing, Nothing]
  final case class &:[A, B](left: Context[A, B], right: Graph[A, B]) extends Graph[A, B] {
    override def toString = left + " &: " + right
  }

  // Extractor that's &v-like
  case class SearchNode[A, B](graph: Graph[A, B], node: Node)
  object FindNode {
    def unapply[A, B](query: SearchNode[A, B]): Option[(Context[A, B], Graph[A, B])] = {
      query.graph.ufold((Option.empty[Context[A, B]], Graph.empty[A, B])) { (memo, context) ⇒
        memo match {
          case (found, graph) if found.isEmpty && context.node == query.node ⇒ (Some(context), graph)
          case (maybeFound, graph) ⇒ (maybeFound, context &: graph)
        }
      } match {
        case (None, _)                       ⇒ None
        case (Some(foundContext), restGraph) ⇒ Some(foundContext, restGraph)
      }
    }
  }

}
