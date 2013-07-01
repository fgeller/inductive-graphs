package graphs.modules

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ModulesTest extends FunSpec with ShouldMatchers {

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
