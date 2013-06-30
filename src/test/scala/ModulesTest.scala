package graphs

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ModulesTest extends FunSpec with ShouldMatchers {

  case class Module(name: String, dependsOn: Set[Module] = Set())
  case class DependencyGraph(modules: Set[Module] = Set()) {
    val graph: Graph[Module, Nothing] = Empty
    def isEmpty = modules isEmpty
    def addModule(newModule: Module) = DependencyGraph(modules + newModule)
    def +(newModule: Module) = addModule(newModule)
    def map(fun: Module ⇒ Module) = DependencyGraph(modules map fun)
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

  }

}
