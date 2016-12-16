package me.hawkweisman.graphite

/**
  * Created by eliza on 12/16/16.
  */
trait EdgeLike[N] {
  def node: N
}

case class WeightedEdge[W: Ordering, N](node: N, weight: W)
extends EdgeLike[N]

case class UnweightedEdge[N](node: N) extends EdgeLike[N]