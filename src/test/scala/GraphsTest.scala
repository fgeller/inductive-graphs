import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers


class GraphsTest extends FunSpec with ShouldMatchers {

  import Graphs._

  describe("Graphs") {

    it("can be empty") {
      Empty.isEmpty should be(true)
      (Context(Seq(), 3, 'c', Seq()) &: Empty).isEmpty should be(false)
    }

    it("can map a function over its nodes") {
      def fun[A, B](ctx: Context[A, B]) = Context(ctx.incoming, ctx.node, "☉", ctx.outgoing)
      (Context(Seq(), 1, 'a', Seq()) &: Empty).gmap(fun).toString should include("☉")
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
