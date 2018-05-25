/**
  * Colors for RBTree nodes
  */
sealed trait Color

case object Red extends Color

case object Black extends Color

/**
  * Red Black tree implementation
  */
class RBTree {

  /**
    * Abstract tree class used for Node and Empty case classes
    *
    * @tparam A Type of the value contained in RBtree
    */
  abstract class Tree[+A: Ordering]()

  /**
    * Case class for representing not empty nodes of the RBTree
    *
    * @param color Color of the Node
    * @param left  Left subtree
    * @param value Value of the node
    * @param right Right subtree
    */
  final case class Node[A: Ordering](color: Color, left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

  /**
    * Case class for representing empty leaves of the RBTree
    */
  final case class Empty[A: Ordering]() extends Tree[A]

  def insert[A: Ordering](value: A, tree: Tree[A]): Tree[A] = {
    def balancedInsert(tree: Tree[A]): Tree[A] = {
      tree match {
        case Empty() => Node(Red, Empty(), value, Empty())
        case Node(color, left, nodeValue, right) =>
          if (value < nodeValue) balanceLeft(color, balancedInsert(left), nodeValue, right)
          else if (value > nodeValue) balanceRight()
          else tree
      }
    }

    def balanceLeft(color: Color, left: Tree[A], value: A, right: Tree[A]): Tree[A] = {
      (color, left, value, right) match {
        case (Black, Node(Red, Node(Red, lllChild, llValue, llrChild), lValue, lrChild), value, Node(Red, rlChild, rValue, rrChild)) =>
          Node(Red, Node(Black, lllChild, llValue, llrChild), lValue, Node(Black, lrChild, value, right))
      }
    }

    def balanceRight(): Tree[A] = {

    }
  }
}