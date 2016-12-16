package me.hawkweisman.graphite


import scala.language.postfixOps

/** Base trait defining a Graph.
  *
  * @tparam V the value to store at each node in this graph
  *
  * Created by eliza on 12/15/16.
  */
trait Graph[V] {

  /** The type of nodes in this graph */
  type Node <: NodeLike
  type Edge <: EdgeLike[Node]

  object Edge {
    def unapply(e: Edge): Node = e.node
  }



  /** Class representing a node in a graph.
    *
    * Each Node stores a set of edges. It may also optionally be associated
    * with a value.
    *
    * @param value the value to store at this node.
    */
  trait NodeLike { self: Node =>
    def value: V
    def edges: Set[Edge]

    /** Add an edge from this node */
    @inline protected[this] def addEdge(e: Edge): Node = ???

    /** Operator for creating an edge from another node to this node.
      *
      * Note that this node is only necessary on directed graphs.
      *
      * @param  that   the node to form an edge to this node.
      */
    @inline final def ~> (that: Edge): Unit = this connectTo that

    def connectTo(that: Edge): Unit = ???

    /** Operator for creating an edge from another node to this node.
      *
      * Note that this node is only necessary on directed graphs.
      *
      * @param  that   the node to form an edge to this node.
      */
    def <~ (that: Edge): Unit = ???

    /**
      * Operator for checking if this node has an edge to another node.
      *
      * @param  that the node to check if this node has an edge to
      * @return true if this node has an edge to that node, false otherwise
      */
    @inline final def ~>? (that: Node): Boolean = this hasEdgeTo that

    /**
      * Operator for checking if another node has an edge to this
      *
      * Note that for undirected graphs, `<~?` is functionally equivalent to
      * `~>?`, while for directed graphs, it is not.
      *
      * @param  that the node to check for an edge to this node
      * @return true if that node has an edge to this node, false otherwise
      */
    @inline final def <~? (that: Node): Boolean = that hasEdgeTo this

    @inline def <~>? (that: Node): Boolean = this <~? that && this ~>? that

    /** Operator for creating a bi-directional edge between this node
      * and another.
      *
      * Note that this node is only necessary on directed graphs.
      *
      * @param  that   the node to form a bi-directional edge with
      */
    @inline final def <~> (that: Edge): Unit = {
      this ~> that
      this <~ that
    }

    final def hasEdgeTo(that: Node): Boolean = edges exists { _.node == that }

    /** The _degree_ (or _valency_) of a node is the number of edges connecting
      * to that node.
      * @return the degree of this node.
      */
    @inline final def degree: Int = nodes count { _ hasEdgeTo this }

    /** Find the shortest path from this node to the specified node.
      *
      * @param to the [[Node]] to find the shortest path to.
      * @return   a list of nodes representing the path (in order)
      */
    def shortestPathTo(to: Node): Seq[Node]
  }

  def nodes: Iterator[Node]

  def edges: Iterator[Edge]
    = for { node <- nodes; edge <- node.edges } yield edge

//  /** @return A sequence of all the [[Node]]s in this graph
//    */
//  @inline final def nodes: Seq[Node] = nodes

  /** Construct and return a new [[Node]].
    *
    * The new node will be in the graph but will not be connected to any other
    * nodes.
    *
    * @return a new [[Node]].
    */
  def node(item: V): Node

  /** The _order_ of a graph is the number of nodes in the graph
    * @return the number of nodes in this graph
    */
  @inline final def graphOrder: Int = nodes length

  /** The _size_ of a graph is the number of edges in the graph
    *
    * Note that this is the graph-theoretic definition of 'size', not the
    * Scala collections definition of size. The `Graph.size()` function
    * (inherited from `Traversable`) is simply the number of items in the
    * graph, as defined by Scala collections.
    *
    * @return the number of edges in this graph
    */
  @inline def graphSize: Int = nodes map { _.edges.size } sum

  /** Find the shortest path from one [[Node]] to another.
    *
    * @param to   the starting [[Node]]
    * @param from the ending [[Node]]
    * @return     a sequence of nodes representing the path (in order)
    */
  @inline final def shortestPath(to: Node, from: Node): Seq[Node]
  = from shortestPathTo to


  /** Search the graph for a [[Node]] corresponding to a given value.
    * @param value the value to find a node for.
    * @return      `Some(Node)` if a matching node was found, `None` otherwise
    */
  @inline final def nodeFor(value: V): Option[Node]
  = nodes find { _.value == value }

//  /** Apply a function to each [[Node]] in this graph.
//    * @param f
//    * @tparam U
//    */
//  @inline final def foreach[U](f: Node => U): Unit
//  = _nodes foreach f

  @inline final def unapply(value: V): Option[Node] = nodeFor(value)
}
object Graph {

  implicit class BuildableValue[V](val self: V) {
    def ~> (other: V) = DiEdgeBuilder(self, other)
    def ~ (other: V) = UnDiEdgeBuilder(self, other)
  }

  trait EdgeBuilder[V, G <: Graph[V]] {
    def to: V
    def from: V
  }

  case class UnDiEdgeBuilder[V, G <: Undirected[V]](to: V, from: V)
  extends EdgeBuilder[V, G]
//
//  trait UnWEdgeBuilder[V, G <: Unweighted[V]]
//  extends EdgeBuilder[V, G]

  trait WEdgeBuilder[V, W]
  extends EdgeBuilder[V, EdgeWeighted[V, W]] {
    def weight: W
  }

  case class WDiEdgeBuilder[V, W: Numeric : Ordering]
  ( to: V, from: V, weight: W)
  extends EdgeBuilder[V, EdgeWeighted.Digraph[V, W]](to, from)
    with WEdgeBuilder[V, W]

  case class DiEdgeBuilder[V, G <: Directed[V]](to: V, from: V)
  extends EdgeBuilder[V, G] {
    def $[W: Numeric : Ordering](weight: W): WDiEdgeBuilder[V, W]
      = WDiEdgeBuilder(to, from, weight)
  }


  def apply[V](ebs: UnDiEdgeBuilder[V, Unweighted.Digraph[V]]*): Unweighted
  .Digraph[V]
  = {

  }

}