package me.hawkweisman.graphite

import me.hawkweisman.graphite.Graph.UnDiEdgeBuilder

/** An unweighted graph.
  *
  * In an unweighted graph, edges are not associated with weights. Finding
  * the shortest path in such a graph cares only about the number of edges,
  * not their lengths.
  *
  * @tparam V the type of the value to associate with each node in the graph.
  * @author Eliza Weisman
  *
  * Created by eliza on 12/15/16.
  */
trait Unweighted[V]
extends Graph[V] {

  override type Node <: UWNode
  override type Edge = UnweightedEdge[Node]

  abstract class UWNode
  extends NodeLike { self: Node =>

    def connectTo(node: Node)

    /** Operator for creating an edge from this node to another.
      * @param  that   the node to form an edge to
      */
    @inline final def <~ (that: Node): Unit
    = that ~> this

  }


}
object Unweighted {
  /** An unweighted directed graph.
    *
    * @tparam V the type of the value to associate with each node in the graph.
    * @author Eliza Weisman
    *
    * Created by eliza on 12/15/16.
    */
  class Digraph[V](val nodes: Set[Unweighted.Digraph[V]#Node])
  extends Unweighted[V]
    with Directed[V]
    with Traversable[Unweighted[V]#Node] {

    override type Node = DirectedUWNode
    override type Edge = Node

    override def node(item: V): Node = ???
//    = {
//      val n = new Node(item)
//      _nodes = _nodes :+ n
//      n
//    }

    class DirectedUWNode(val value: V, val edges: Set[Edge])
    extends UWNode
      with DirectedNode { self: Node =>

      /** @inheritdoc
        *
        * In an unweighted graph, the shortest path is the path that requires
        * the fewest edges to be traversed.
        */
      def shortestPathTo(to: Node): Seq[Node] = ???

    }

    /** Apply a function to each [[Node]] in this graph.
      *
      * @param f
      * @tparam U
      */
    @inline override final def foreach[U](f: (Unweighted[V]#Node) => U): Unit
      = nodes foreach f

  }

  type UndigraphBuilders[V]
    = Seq[UnDiEdgeBuilder[V, Undigraph[V]]]

  /** An unweighted directed graph.
    *
    * @tparam V the type of the value to associate with each node in the graph.
    * @author Eliza Weisman
    *
    * Created by eliza on 12/15/16.
    */
  class Undigraph[V](builders: UndigraphBuilders[V])
  extends Unweighted[V]
    with Undirected[V]
    with Traversable[Unweighted[V]#Node] {

    override type Node = UndirectedUWNode

    val nodes: Set[Node] = {
      def _initialize( edges: Map[V, Set[V]]
                     , inits:    UndigraphBuilders[V]): Set[Node]
        = inits match {
            case Seq() =>
              val newNodes: Map[V, Node] = Map(edges.keySet map { v =>
                (v, new UndirectedUWNode(v))
              })
              for { (v, node: Node) <- newNodes
                    toValue <- edges(v)
                    toNode <- newNodes(toValue)}
              yield {
                node.edges += UnweightedEdge(toNode)
                node
              }
//              for { (value, outEdges) <- ns } yield { new Node(value, )}
            case UnDiEdgeBuilder(to: V, from: V) +: rest =>
              _initialize( edges updated(from, edges.getOrElse(from, Set())+to)
                         , rest)
        }

      _initialize(Map[V, Set[V]](), builders)
    }

    override def node(item: V): Node = ???
//    = {
//      val n = new Node(item)
//      n
//    }

    class UndirectedUWNode(val value: V)
      extends UWNode
        with UndirectedNode { self: Node =>
        def edges: Set[Edge] = _edges

        private[Undigraph] def edges_=(e: Edge): Unit = { _edges += e }

        private[this] var _edges: Set[Edge] = Set()

      /** @inheritdoc
        *i
        * In an unweighted graph, the shortest path is the path that requires
        * the fewest edges to be traversed.
        */
      def shortestPathTo(to: Node): Seq[Node] = ???
      def connectTo(that: Node): Node = ???
    }

    /** Apply a function to each [[Node]] in this graph.
      *
      * @param f
      * @tparam U
      */
    @inline override final def foreach[U](f: (Unweighted[V]#Node) => U): Unit
    = nodes foreach f

    /** @inheritdoc
      *
      * This is special-cased for undirected graphs, since they cannot calculate
      * the number of edges in the same way as directed graphs.
      */
    @inline override def graphSize: Int = super.graphSize / 2


  }

}