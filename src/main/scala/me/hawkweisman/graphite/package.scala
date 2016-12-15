package me.hawkweisman
/** == Graphite ==
  *
  * This package contains two subpackages, one for edge-weighted graphs, and
  * one for graphs that are unweighted. Each package contains its own concrete
  * implementations for directed ([[graphite.Unweighted.Digraph]] and
  * [[graphite.EdgeWeighted.Digraph]] and undirected (
  * [[graphite.EdgeWeighted.Undigraph]] and [[graphite.Unweighted.Undigraph]])
  * graphs.
  *
  * All graph implementations extend the core [[graphite.Graph]] trait.
  *
  * Nodes in the graphs in this package may be associated with a value. If
  * you do not wish to store a value in the nodes in your graph, simply
  * create graphs with the value type set to `Unit`.
  *
  * @author Eliza Weisman
  *
  * Created by eliza on 12/15/16.
  */
package object graphite {

}
