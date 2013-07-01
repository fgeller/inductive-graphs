package graphs

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ModulesTest extends FunSpec with ShouldMatchers {
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

  describe("A module") {

    it("has a name") {
      val testName = "test"
      Module(name = testName).name should be("test")
    }

    it("depends on other modules") {
      val leafModule = Module(name = "leaf")
      leafModule.dependsOn should be('empty)

      val hans = Module(name = "hans", dependsOn = Set(leafModule))
      hans.dependsOn should be(Set(leafModule))
    }

  }

  describe("A dependency graph") {

    it("starts out empty") {
      val dependencyGraph = DependencyGraph()
      dependencyGraph.isEmpty should be(true)
    }

    it("holds modules") {
      val testModules = Set(Module(name = "leaf"))
      val graph = DependencyGraph(modules = testModules)
      graph.modules should be(testModules)
    }

    it("can add modules") {
      val initialModules = Set(Module(name = "leaf"))
      val newModule = Module(name = "new")
      val graph = DependencyGraph(modules = initialModules) + newModule
      graph.modules should be(initialModules + newModule)
    }

    it("can be mapped over") {
      val testModules = Set(Module("a"), Module("b"))
      val testGraph = DependencyGraph(testModules)
      def fun(mod: Module) = Module(name = mod.name + mod.name)
      val expectedGraph = DependencyGraph(testModules map fun)

      testGraph.map(fun) should be(expectedGraph)
    }

    it("knows nodes by name") {
      val modA = Module("a")
      val modB = Module("b")
      val testGraph = DependencyGraph(Set(modA, modB))

      testGraph.find(modA.name) should be(Some(modA))
      testGraph.find(modB.name) should be(Some(modB))
      testGraph.find(this.toString) should be(None)
    }

    it("finds dependants") {
      val modA = Module("mod a")
      val modB = Module("mod b", Set(modA))
      val testGraph = DependencyGraph(Set(modA, modB))

      testGraph.dependants(modA.name) should be(Set(modB))
      testGraph.dependants(modB.name) should be(Set())
      intercept[NoSuchElementException] { testGraph.dependants(this.toString) }
    }

    it("can be printed in dot format") {
      val modA = Module("mod a")
      val modB = Module("mod b", Set(modA))
      val testGraph = DependencyGraph(Set(modA, modB))

      testGraph.asDot should be("""digraph g {
"mod b" -> "mod a";
}""")
    }

  }

}
