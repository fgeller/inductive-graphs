object Graphs extends App {

  type Node = Int
  type Adj[B] = Seq[(B, Node)]
  type Context[A, B] = (Adj[B], Node, A, Adj[B])

  sealed trait Graph[A, B] {

  }

}
