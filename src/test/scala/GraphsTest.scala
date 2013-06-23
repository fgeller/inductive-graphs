import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers


class GraphsTest extends FunSpec with ShouldMatchers {

  import Graphs._

  describe("Context") {
    it("knows the successor nodes") {
      Context(Seq(), 1, 1, Seq((2, 2))).suc should be (Seq(2))
      Context(Seq(), 1, 1, Seq()).suc should be (Seq())
    }
  }

  describe("Graphs") {

    it("can be empty") {
      Empty.isEmpty should be(true)
      (Context(Seq(), 3, 'c', Seq()) &: Empty).isEmpty should be(false)
    }

    it("can map a function over its nodes") {
      def fun[A, B](ctx: Context[A, B]) = Context(ctx.incoming, ctx.node, "☉", ctx.outgoing)
      (Context(Seq(), 1, 'a', Seq()) &: Empty).gmap(fun).toString should include("☉")
    }

    it("can reverse edge directions") {
      val g = (Context(Seq(("left", 2)), 1, 'a', Seq()) &: Context(Seq(), 2, 'b', Seq()) &: Empty)
      g.toString should include("[(left,2)]→")
      g.grev.toString should include("→[(left,2)]")
    }

    it("can make a graph undirectional") {
      val g = (Context(Seq(("left", 2)), 1, 'a', Seq()) &: Context(Seq(), 2, 'b', Seq()) &: Empty)
      g.undir.toString should include("[(left,2)]→")
      g.undir.toString should include("→[(left,2)]")
    }

    it("can use fold to sum up values") {
      val g = (Context(Seq(("left", 2)), 1, 23, Seq()) &:
        Context(Seq(), 2, 23, Seq()) &:
        Empty)
      g.ufold(0) { (memo, ctx) ⇒ memo + ctx.value } should be(46)
    }

    it("can use an extractor to find a context") {
      val testGraph = (Context(Seq(("left", 2)), 1, 23, Seq()) &:
        Context(Seq(), 2, 46, Seq()) &:
        Empty)
      val testNode = 2

      SearchNode(testGraph, testNode) match {
        case FindNode(Context(in, 2, label, out), restGraph) ⇒
          in should be(out)
          label should be (46)
          restGraph should not be testGraph
        case _ ⇒ fail("should find the context for node 2")
      }
    }

    it("knows the degree of a node") {
      val testGraph = (Context(Seq(("left", 2)), 1, 23, Seq()) &:
        Context(Seq(), 2, 46, Seq()) &:
        Empty)

      testGraph.degree(2) should be (Some(0))
      testGraph.degree(1) should be (Some(1))
      testGraph.degree(0) should be (None)
    }

    it("finds the successors of a node") {
      val testGraph = (Context(Seq(), 1, 23, Seq(("left", 2))) &:
        Context(Seq(), 2, 46, Seq()) &:
        Empty)

      testGraph.gsuc(1) should be (Seq(2))
      testGraph.gsuc(2) should be (Seq())
    }

    it("can delete a node") {
      val testGraph = (Context(Seq(("left", 2)), 1, 23, Seq()) &:
        Context(Seq(), 2, 46, Seq()) &:
        Empty)

      testGraph.delete(1) should be (Context(Seq(), 2, 46, Seq()) &: Empty)
      testGraph.delete(2) should be (Context(Seq(("left", 2)), 1, 23, Seq()) &: Empty)
      testGraph.delete(0) should be (testGraph)
    }

    it("knows its nodes") {
      Empty.nodes should be('empty)
      (Context(Seq(("left", 2)), 1, 23, Seq()) &:
        Context(Seq(), 2, 23, Seq()) &:
        Empty).nodes should be (Seq(2, 1))
    }

    it("can reconstruct the base graph from figure 1") {
      val g = Context(Seq(("left", 2), ("up", 3)), 1, 'a', Seq(("right", 2))) &:
        Context(Seq(), 2, 'b', Seq(("down", 3))) &:
      Context(Seq(), 3, 'c', Seq()) &: Empty

      g.toString should endWith("&: Empty")
      Seq(1, 2, 3) map(_.toString) foreach { node ⇒
        g.toString should include(node)
      }
    }

  }

}
