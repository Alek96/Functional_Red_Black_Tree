/**
  * Colors for Red Black tree nodes
  */
sealed abstract class Color

case object Red extends Color

case object Black extends Color

class RBTree {

  abstract class Tree[+A]

  case class Node[A](color: Color, left: Tree[A], value: A, right: Tree[A]) extends Tree[A]
  case class Empty[A]() extends Tree[A]

}