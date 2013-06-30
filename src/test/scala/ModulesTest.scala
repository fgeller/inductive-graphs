package graphs

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ModulesTest extends FunSpec with ShouldMatchers {

  describe("Dependency graphs") {

    case class Module(name: String, dependsOn: Set[Module])
    case class DependencyGraph(graph: Graph[Module, Nothing] = Empty) {
      def isEmpty = graph isEmpty
    }

    it("start out empty") {
      val dependencyGraph = DependencyGraph()
      dependencyGraph.isEmpty should be(true)
    }

  }

}
