package me.hawkweisman.graphite

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
  override type Edge = Node

  abstract class UWNode(value: V)
  extends NodeLike(value) { self: Node =>

    def connectTo(node: Node)

    /** Operator for creating an edge from this node to another.
      * @param  that   the node to form an edge to
      */
    @inline final def <~ (that: Node): Unit
    = that ~> this

    override def hasEdgeTo(node: Node): Boolean
    = _edges contains node
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
  class Digraph[V]
  extends Unweighted[V]
    with Directed[V]
    with Traversable[Unweighted[V]#Node] {

    override type Node = DirectedUWNode
    override type Edge = Node

    override def node(item: V): Node = {
      val n = new Node(item)
      _nodes = _nodes :+ n
      n
    }

    class DirectedUWNode(value: V)
    extends UWNode(value)
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
      = _nodes foreach f

  }

  /** An unweighted directed graph.
    *
    * @tparam V the type of the value to associate with each node in the graph.
    * @author Eliza Weisman
    *
    * Created by eliza on 12/15/16.
    */
  class Undigraph[V]
  extends Unweighted[V]
    with Undirected[V]
    with Traversable[Unweighted[V]#Node] {

    override type Node = UndirectedUWNode
    override type Edge = Node

    override def node(item: V): Node = {
      val n = new Node(item)
      _nodes = _nodes :+ n
      n
    }

    class UndirectedUWNode(value: V)
      extends UWNode(value)
        with UndirectedNode { self: Node =>

      /** @inheritdoc
        *i
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
    = _nodes foreach f

    /** @inheritdoc
      *
      * This is special-cased for undirected graphs, since they cannot calculate
      * the number of edges in the same way as directed graphs.
      */
    @inline override def graphSize: Int = super.graphSize / 2


  }

}