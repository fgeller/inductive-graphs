package graphs.modules

import graphs._
import graphs.Types.Node

case class Module(name: String, dependsOn: Set[Module] = Set()) {
  override def toString = s"Module($name depends on (${dependsOn.map(_.name) mkString ", "}))"
}

case class DependencyGraph(modules: Set[Module] = Set()) {

  val nodeByName = {
    val nodesAndNames = (1 to modules.size).zip(modules.map(_.name))
    nodesAndNames map { case (node, name) ⇒ name → node } toMap
  }

  val sortedContexts = {
    type ModuleNode = NodeContext[String, String]
    type Nodes = Seq[ModuleNode]
    def knowsOutgoing(known: Nodes, node: ModuleNode) =
      node.out.forall(edge ⇒ known.exists(_.node == edge.node))

    def rec(toSort: Nodes, sorted: Nodes, loopFlag: Option[(ModuleNode, Nodes)] = None): Nodes = toSort.toList match {
      case Nil                                         ⇒ sorted
      case head :: tail if knowsOutgoing(sorted, head) ⇒ rec(tail, sorted :+ head)
      case head :: _ if loopFlag == Some(head, sorted) ⇒ throw new IllegalArgumentException("Caught in a loop.")
      case head :: tail                                ⇒ rec(tail :+ head, sorted, Some((head, sorted)))
    }

    val nodesContexts: Seq[NodeContext[String, String]] = modules.map { mod ⇒
      val outgoingEdges = mod.dependsOn.map(parent ⇒ Edge(parent.name, nodeByName(parent.name))).toSeq
      Context(Seq(), nodeByName(mod.name), mod.name, outgoingEdges)
    }.toSeq

    rec(nodesContexts, Seq())
  }

  val graph = (Graph.empty[String, String] /: sortedContexts) { (memo, context) ⇒ context &+: memo }

  def isEmpty = modules isEmpty
  def addModule(newModule: Module) = DependencyGraph(modules + newModule)
  def +(newModule: Module) = addModule(newModule)
  def map(fun: Module ⇒ Module) = DependencyGraph(modules map fun)
  def find(name: String): Option[Module] = modules.find(_.name == name)
  def dependants(name: String): Set[Module] = {
    if (!modules.exists(_.name == name)) throw new NoSuchElementException
    graph.children(nodeByName(name)).flatMap(node ⇒ find(node.value)).toSet
  }
  def asDot = Graph.asDot(graph)
}
