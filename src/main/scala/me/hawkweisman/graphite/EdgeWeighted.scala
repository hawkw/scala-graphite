package me.hawkweisman.graphite

import org.scalactic.Requirements

import scala.io.Source
import scala.{specialized => sp}
//import scala.languageFeature.implicitConversions
import scala.language.postfixOps

import Ordering.Implicits._

/** A graph that is edge-weighted.
  *
  * In an edge-weighted graph, each edge is associated with a weight. These
  * weights can be any type which is an instance of `Numeric`. This class is
  * specialised over `Int`, `Long`, `Float`, and `Double`, but you can use
  * anything that has an associated implementation of `Numeric`.
  *
  * @tparam V      the type of the value to associate with each node in the
  *                graph.
  * @tparam Weight the type of the weight value associated with each edge in
  *                the graph.
  *
  * Created by eliza on 12/15/16.
  */
abstract class EdgeWeighted[V, Weight: Numeric: Ordering]
extends Graph[V] {

  override type Node <: EWNode
  override type Edge = WeightedEdge[Node, Weight]
//  @inline protected[this] implicit def edge2node(e: Edge): Node
//    = e._1

  abstract class EWNode(value: V, edges: Set[Edge])
    extends NodeLike(value, edges) { self: Node =>

    @inline override final def <~ (edge: Edge): Unit = {
//      val (that, weight) = edge
//      that ~> (this, weight)
      ???
    }

    /** Connect this node to another node with an edge with the given weight.
      *
      * The weight must be greater than zero.
      *
      * @param edge the edge to add
      */
    @throws[IllegalArgumentException]("if the weight is <= 0")
    override protected[this] def addEdge (edge: Edge): Unit = ???
//    = { val (_, weight: Weight) = edge
//      require(weight > implicitly[Numeric[Weight]].zero)
//      _edges += edge
//    }

//    @inline override def hasEdgeTo(node: Node): Boolean
//    = _edges exists { case (n, _) => n == node }

    /** Returns the [[Weight]] of the edge to the given [[Node]], if one exists.
      *
      * @param node the [[Node]] to find the weight of the edge to
      * @return     `Some(Weight)` if this node has an edge to the given node,
      *             `None` otherwise.
      */
    final def weightTo(node: Node): Option[Weight]
    = ???
//    = _edges find { case (n, _) => n == node } map { case (_, w) => w }
  }

}

/** ==Edge-Weighted Graphs==
  *
  * In an edge-weighted graph, each edge is associated with a weight. These
  * weights can be any type which is an instance of `Numeric`. This class is
  * specialised over `Int`, `Long`, `Float`, and `Double`, but you can use
  * anything that has an associated implementation of `Numeric`.
  *
  * The shortest paths between nodes in an edge-weighted graph are found
  * based on the weight of the edges traversed, not by the number of edges in
  * the path.
  */
object EdgeWeighted {

  /** An edge-weighted directed graph.
    *
    * @tparam V      the type of the value to associate with each node in the
    *                graph.
    * @tparam Weight the type of the weight value associated with each edge in
    *                the graph.
    * @author        Eliza Weisman
    */
  class Digraph[V, @sp(Int, Long, Float, Double) Weight : Numeric : Ordering]
  (val nodes: Set[Node])
  extends EdgeWeighted[V, Weight]
    with Directed[V]
    with Traversable[EdgeWeighted[V, Weight]#Node]
    with Requirements {

    override type Node = DirectedEWNode

    override def node(item: V): Node = {
      ???
//      val n = new Node(item)
//      _nodes = _nodes :+ n
//      n
    }

    class DirectedEWNode(value: V, edges: Set[Edge])
      extends EWNode(value, edges)
        with DirectedNode { self: Node =>

      /** @inheritdoc
        *
        * In an edge-weighted graph, the shortest path is the path for which
        * the sum of the weights of the edges traversed is the lowest.
        */
      def shortestPathTo(to: Node): Seq[Node] = ???
    }

    /** Apply a function to each [[Node]] in this graph.
      * @param f
      * @tparam U
      */
    @inline final def foreach[U](f: (EdgeWeighted[V, Weight]#Node) => U): Unit
      = nodes foreach f

  }

  object Digraph {
    /**
      * Parse an edge-weighted directed graph.
      *
      * Parse an edge-weighted directed graph from the edge-weighted graph
      * description format from _Algorithms_, 4th Edition.
      *
      * @param mkValue a function that produces the values to place in each node.
      * @param source  a `Source` containing the text to parse
      * @tparam V      the type of the values in the graph
      * @tparam W      the type of the weights in the graph (note that this
      *                should probably be either `Double` or `Float`, since
      *                most of the weights in the _Algorithms_ 4th Ed. sample
      *                files are decimal and truncating them to integers would
      *                give you a lot of zeros)
      * @return        a graph based on the one in the data file.
      * @author        Aubrey Collins & Eliza Weisman
      */
    def parse[V, W : Numeric](mkValue: () => V)(source: Source): Digraph[V, W]
    = {
      val graph = new Digraph[V, W]()

      // val f = Source.fromFile(new java.io.File(path))

      // read file as a sequence of strings
      val lines: Seq[String] = source.getLines.toIndexedSeq
      // the first line in the input file is the number of nodes
      val numNodes = lines.headOption map (_ toInt) getOrElse 0
      // create an array to store the new nodes and fill it with empty nodes
      val nodes = for { _ <- 0 until numNodes } yield graph.node(mkValue())

      //parse rest of file and make connections
      for { ln <- lines.drop(2)
            Array(to, from, weight) = ln split ' '
      } {
        nodes(to toInt) ~> (nodes(from toInt), weight.toDouble.asInstanceOf[W])
      }

      graph
    }
  }

  /** An edge-weighted undirected graph.
    *
    * @tparam V      the type of the value to associate with each node in the
    *                graph.
    * @tparam Weight the type of the weight value associated with each edge in
    *                the graph.
    * @author        Eliza Weisman
    */
  class Undigraph[V, @sp(Int, Long, Float, Double) Weight : Numeric : Ordering]
  extends EdgeWeighted[V, Weight]
    with Undirected[V]
    with Traversable[EdgeWeighted[V, Weight]#Node]  {

    override type Node = UndirectedEWNode
//    override type Edge = (Node, Weight)

    override def node(item: V): Node = {
      val n = new Node(item)
      _nodes = _nodes :+ n
      n
    }

    class UndirectedEWNode(value: V, edges: Set[Edge])
      extends EWNode(value)
        with UndirectedNode { self: Node =>

      /** @inheritdoc
        *
        * In an edge-weighted graph, the shortest path is the path for which
        * the sum of the weights of the edges traversed is the lowest.
        */
      def shortestPathTo(to: Node): Seq[Node] = ???

    }

    /** Apply a function to each [[Node]] in this graph.
      *
      * @param f
      * @tparam U
      */
    @inline
    override final def foreach[U](f: (EdgeWeighted[V, Weight]#Node) => U): Unit
      = nodes foreach f

    /** @inheritdoc
      *
      * This is special-cased for undirected graphs, since they cannot calculate
      * the number of edges in the same way as directed graphs.
      */
    @inline override def graphSize: Int = super.graphSize / 2

  }

  object Undigraph {

    /**
      * Parse an edge-weighted undirected graph.
      *
      * Parse an edge-weighted undirected graph from the edge-weighted graph
      * description format from _Algorithms_, 4th Edition.
      *
      * @param mkValue a function that produces the values to place in each node.
      * @param source  a `Source` containing the text to parse
      * @tparam V      the type of the values in the graph
      * @tparam W      the type of the weights in the graph (note that this
      *                should probably be either `Double` or `Float`, since
      *                most of the weights in the _Algorithms_ 4th Ed. sample
      *                files are decimal and truncating them to integers would
      *                give you a lot of zeros)
      * @return        a graph based on the one in the data file.
      * @author        Aubrey Collins & Eliza Weisman
      */
    def parse[V, W : Numeric](mkValue: () => V)(source: Source): Undigraph[V, W]
    = {
      val graph = new Undigraph[V, W]()

      // read file as a sequence of strings
      val lines: Seq[String] = source.getLines.toIndexedSeq
      // the first line in the input file is the number of nodes
      val numNodes = lines.headOption map (_ toInt) getOrElse 0
      // create an array to store the new nodes and fill it with empty nodes
      val nodes
      = for { i <- 0 until numNodes } yield graph.node(mkValue())

      //parse rest of file and make connections
      for { ln <- lines.drop(2)
        Array(to, from, weight) = ln.split(' ')
      } {
        nodes(to.toInt) ~> (nodes(from.toInt), weight.toDouble.asInstanceOf[W])
      }

      graph
    }
  }
}