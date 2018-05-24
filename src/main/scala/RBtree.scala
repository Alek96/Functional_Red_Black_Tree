/**
  * Colors for RBTree nodes
  */
sealed trait Color

case object Red extends Color

case object Black extends Color

/**
  *Red Black tree implementation
  */
class RBTree {

  /**
    * Abstract tree class used for Node and Empty case classes
    * @tparam A Type of the value contained in RBtree
    */
  abstract class Tree[+A]()

  /**
    * Case class for representing not empty nodes of the RBTree
    * @param color Color of the Node
    * @param left Left subtree
    * @param value Value of the node
    * @param right Right subtree
    */
  final case class Node[A <% Ordered[A]](color: Color, left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

  /**
    * Case object for representing empty leaves of the RBTree
    */
  final case object Empty extends Tree[Nothing] {
    def color: Color = Black
  }

  /**
    * Function for getting empty RBTree instance
    * @return Returns empty tree
    */
  def empty[A]: Tree[A] = Empty

}