package com.intenthq.challenge

case class Node(value: Int, edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false
  def run(source: Node, target: Node): Boolean = {
    val reachable = DFS(source)
    reachable.contains(target.value)
  }

  /*
    Notes:

    A DFS/BFS approach is required to support cyclic graphs, which
    the codewars problem says should be considered.

    This solution assumes that the value field of each Node functions
    as a unique ID. If that's not the case it's possible solve this
    problem a different way with very minor modifications (compare/track
    the JVM reference instead of the node value)
  */

  // Use iterative approach
  import scala.collection.mutable.Stack
  def DFS(source: Node): List[Int] = {
    val visited = Stack[Int]()
    val fringe = Stack[Node]()
    visited.push(source.value)
    fringe.pushAll(source.edges)
    while(fringe.nonEmpty) {
      val curr = fringe.pop()
      visited.push(curr.value)
      for(curr_edge <- curr.edges){
        if(!visited.contains(curr_edge.value) && !fringe.contains(curr_edge))
          fringe.push(curr_edge)
      }
    }
    visited.toList
  }

  // Use tail recursive approach
  import scala.collection.immutable.{Stack => ImStack}
  def DFS_tailrec(source: Node): List[Int] = {
    val visited = ImStack[Int](source.value)
    val fringe = ImStack[Node](source.edges:_*)
    recurse(source, visited, fringe)
  }

  @scala.annotation.tailrec
  def recurse(source: Node, visited: ImStack[Int], fringe: ImStack[Node]): List[Int] = {
    if(fringe.nonEmpty) {
      val (curr, newFringe) = fringe.pop2
      val nextSeen = visited.push(curr.value)
      val (nextFringe) = curr.edges.foldLeft(newFringe) { (fr, e) =>
        val never_seen = !nextSeen.contains( e ) && !fr.contains( e )
        if(never_seen)
          fr.push(e)
        else
          fr
      }
      recurse(source, nextSeen, nextFringe)
    } else
      visited.toList
  }

}
