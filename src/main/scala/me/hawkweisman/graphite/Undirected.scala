package me.hawkweisman.graphite

/**
  * Created by eliza on 12/15/16.
  */
trait Undirected[V]
extends Graph[V] {
  override type Node <: UndirectedNode

  trait UndirectedNode
  extends NodeLike { self: Node =>

    override final def connectTo(edge: Edge): Unit
      = if (!this.edges.contains(edge)) {
        this.addEdge(edge)
        this <~ edge
      }

  }

}