package graphs

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ModulesTest extends FunSpec with ShouldMatchers {
  case class Module(name: String, dependsOn: Set[Module] = Set())
  case class DependencyGraph(modules: Set[Module] = Set()) {
    val graph: Graph[Module, Nothing] = Empty
    def isEmpty = modules isEmpty
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

  describe("Dependency graphs") {

    it("start out empty") {
      val dependencyGraph = DependencyGraph()
      dependencyGraph.isEmpty should be(true)
    }

    it("hold modules") {
      val testModules = Set(Module(name = "leaf"))
      val graph = DependencyGraph(modules = testModules)
      graph.modules should be(testModules)
    }
  }

}
