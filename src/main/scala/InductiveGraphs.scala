object Graphs extends App {

  type Node = Int
  type Adj[+B] = Seq[(B, Node)]
  case class Context[+A, +B](incoming: Adj[B], node: Node, value: A, outgoing: Adj[B]) {
    def suc = outgoing.map(_._2).toSet
    override def toString = s"([${incoming mkString " "}]→ $node($value) →[${outgoing mkString " "}])"
  }

  object Graph {
    def empty[A, B]: Graph[A, B] = Empty
    def asDot(graph: Graph[_, _]) = {
      def findValue(node: Node) = SearchNode(graph, node) match {
        case FindNode(Context(_, _, value, _), _) ⇒ value
      }
      val edges = graph.ufold(Set.empty[Any]) { (memo, context) ⇒
        memo ++
          context.incoming.map(i ⇒ (findValue(i._2), context.value)) ++
          context.outgoing.map(o ⇒ (context.value, findValue(o._2)))
      } map { case (from, to) ⇒ s"$from -> $to;\n" } mkString

      s"digraph g {\n$edges}"
    }
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

    def nodes: Set[Node] = this match {
      case Empty                          ⇒ Set()
      case &:(left: Context[A, B], right) ⇒ right.ufold(Set(left.node)) { (memo, ctx) ⇒ memo + ctx.node }
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
      case FindNode(Context(_, _, _, out), _) ⇒ out.map(_._2).toSet
      case _                                  ⇒ Set()
    }

    def leaves: Set[Node] = this.ufold(this.nodes) { (memo, context) ⇒
      memo diff context.incoming.map(_._2).toSet
    }

    def roots: Set[Node] = this.ufold(this.nodes) { (memo, context) ⇒
      if (context.incoming.isEmpty) memo
      else memo - context.node
    }

    def topologicallySorted(toVisit: List[Node]): List[Node] = {
      if (toVisit.isEmpty || this.isEmpty) Nil
      else SearchNode(this, toVisit.head) match {
        case FindNode(context, _) ⇒
          val sorted = topologicallySorted((context.incoming.map(_._2).toList) ++ toVisit.tail)
          if (sorted contains toVisit.head) sorted
          else toVisit.head :: sorted
        case _ ⇒ topologicallySorted(toVisit tail)
      }
    }
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
