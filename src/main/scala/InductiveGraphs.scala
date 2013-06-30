package graphs

object Types {
  type Node = Int
  type Edges[+B] = Seq[HalfEdge[B]]
}

import Types._

trait HalfEdge[+B] {
  def value: B
  def node: Node
}

trait NodeContext[+A, +B] {
  def in: Edges[B]
  def out: Edges[B]
  def node: Node
  def value: A

  def suc = out.map(_.node).toSet
}

object NodeContext {
  def unapply[A, B](ctx: NodeContext[A, B]): Option[(Edges[B], Node, A, Edges[B])] = Some((ctx.in, ctx.node, ctx.value, ctx.out))
}

case object Empty extends Graph[Nothing, Nothing]

case class Context[+A, +B](in: Edges[B], node: Node, value: A, out: Edges[B]) extends NodeContext[A, B] {
  override def toString = s"([${in.map(e ⇒ (e.value, e.node)) mkString " "}]→ $node($value) →[${out.map(e ⇒ (e.value, e.node)) mkString " "}])"
}

object Graph {

  def empty[A, B]: Graph[A, B] = Empty

  def asDot(graph: Graph[_, _]) = {
    def findValue(node: Node) = SearchNode(graph, node) match {
      case FoundNode(NodeContext(_, _, value, _), _) ⇒ value
    }
    val edges = graph.ufold(Set.empty[Any]) { (memo, context) ⇒
      memo ++
        context.in.map(i ⇒ (findValue(i.node), context.value)) ++
        context.out.map(o ⇒ (context.value, findValue(o.node)))
    } map { case (from, to) ⇒ s"$from -> $to;\n" } mkString

    s"digraph g {\n$edges}"
  }
}

case class Edge[+B](value: B, node: Node) extends HalfEdge[B]

trait Graph[+A, +B] {

  def &:[C >: A, D >: B](context: NodeContext[C, D]): Graph[C, D] =
    pair.PairGraph(context, this)

  def &+:[C >: A, D >: B](context: NodeContext[C, D]): Graph[C, D] =
    pair.PairGraph(context, this)

  def context[C >: A, D >: B](in: Edges[D], node: Node, value: C, out: Edges[D]): NodeContext[C, D] =
    Context(in, node, value, out)

  def isEmpty: Boolean = this match {
    case Empty ⇒ true
    case _     ⇒ false
  }

  def gmap[A, B](f: NodeContext[A, B] ⇒ NodeContext[A, B]): Graph[A, B] = this match {
    case Empty                              ⇒ Empty
    case &:(left: NodeContext[A, B], right) ⇒ f(left) &: right.gmap(f)
  }

  def grev: Graph[A, B] = gmap { left: NodeContext[A, B] ⇒
    context(left.out, left.node, left.value, left.in)
  }

  def ufold[C](memo: C)(f: (C, NodeContext[A, B]) ⇒ C): C = this match {
    case Empty ⇒ memo
    case &:(left: NodeContext[A, B], right: Graph[A, B]) ⇒ right.ufold(f(memo, left))(f)
  }

  def nodes: Set[Node] = this match {
    case Empty ⇒ Set()
    case &:(left: NodeContext[A, B], right: Graph[A, B]) ⇒ right.ufold(Set(left.node)) { (memo, ctx) ⇒ memo + ctx.node }
  }

  def undir: Graph[A, B] = gmap { ctx: NodeContext[A, B] ⇒
    context(ctx.in ++ ctx.out, ctx.node, ctx.value, ctx.in ++ ctx.out)
  }

  def degree(node: Node) = SearchNode(this, node) match {
    case FoundNode(NodeContext(in, _, _, out), _) ⇒ Some(in.size + out.size)
    case _                                        ⇒ None
  }

  def delete(node: Node) = SearchNode(this, node) match {
    case FoundNode(_, restGraph) ⇒ restGraph
    case _                       ⇒ this
  }

  def gsuc(node: Node) = SearchNode(this, node) match {
    case FoundNode(NodeContext(_, _, _, out), _) ⇒ out.map(_.node).toSet
    case _                                       ⇒ Set()
  }

  def roots: Set[Node] = this.ufold(this.nodes) { (memo, context) ⇒
    memo diff context.in.map(_.node).toSet
  }

  def leaves: Set[Node] = this.ufold(this.nodes) { (memo, context) ⇒
    if (context.in.isEmpty) memo
    else memo - context.node
  }

  def nodeContext(node: Node) = SearchNode(this, node) match {
    case FoundNode(context, _) ⇒ Some(context)
    case _                     ⇒ None
  }

  private def findIncoming(toVisit: List[Node]): List[NodeContext[A, B]] =
    if (toVisit.isEmpty || this.isEmpty) Nil
    else SearchNode(this, toVisit.head) match {
      case FoundNode(context, _) ⇒
        val sorted = findIncoming((context.in.map(_.node).toList) ++ toVisit.tail)
        if (sorted contains context) sorted
        else context :: sorted
      case _ ⇒ findIncoming(toVisit tail)
    }

  def children(node: Node): List[NodeContext[A, B]] = SearchNode(this, node) match {
    case FoundNode(context, _) ⇒ findIncoming(context.in.map(_.node).toList)
    case _                     ⇒ Nil
  }

}

// Extractor that's &v-like
case class SearchNode[A, B](graph: Graph[A, B], node: Node)
object FoundNode {
  def unapply[A, B](query: SearchNode[A, B]): Option[(NodeContext[A, B], Graph[A, B])] = {
    query.graph.ufold((Option.empty[NodeContext[A, B]], Graph.empty[A, B])) { (memo, context) ⇒
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

trait &:[+A, +B] {
  def left: NodeContext[A, B]
  def right: Graph[A, B]
}

object &: {
  def unapply[A, B](and: &:[A, B]): Option[(NodeContext[A, B], Graph[A, B])] = Some((and.left, and.right))
}

package pair {

  case class PairGraph[A, B](left: NodeContext[A, B], right: Graph[A, B]) extends Graph[A, B] with &:[A, B] {
    override def &:[C >: A, D >: B](context: NodeContext[C, D]) = PairGraph(context, this)
    override def &+:[C >: A, D >: B](context: NodeContext[C, D]) = PairGraph(context, updateNodes(context))
    override def toString = left + " &: " + right

    private def updateEdges[D >: B](newNode: Node, newEdges: Edges[D], oldNode: Node, oldEdges: Edges[D]) =
      newEdges.find(_.node == oldNode) map { edge: HalfEdge[D] ⇒
        Edge(edge.value, newNode) +: oldEdges
      } getOrElse oldEdges

    private def updateNodes[C >: A, D >: B](newContext: NodeContext[C, D]) = this.gmap { original: NodeContext[C, D] ⇒
      Context(
        updateEdges(newContext.node, newContext.out, original.node, original.in),
        original.node,
        original.value,
        updateEdges(newContext.node, newContext.in, original.node, original.out)
      )
    }

  }

}
