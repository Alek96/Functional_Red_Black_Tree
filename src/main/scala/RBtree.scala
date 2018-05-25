import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

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
  abstract class Tree[+A]()

  /**
    * Case class for representing not empty nodes of the RBTree
    *
    * @param color Color of the Node
    * @param left  Left subtree
    * @param value Value of the node
    * @param right Right subtree
    */
  final case class Node[+A](color: Color, left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

  /**
    * Case class for representing empty leaves of the RBTree
    */
  final case object Empty extends Tree[Nothing]

  //https://course.ccs.neu.edu/cs3500wc/jfp99redblack.pdf
  def insert[A: Ordering](value: A, tree: Tree[A]): Tree[A] = {
    def ins(tree: Tree[A]): Tree[A] = {
      tree match {
        case Empty => Node(Red, Empty, value, Empty)

        case Node(color, left, nodeValue, right) =>
          if (value < nodeValue) balance(color, ins(left), nodeValue, right)
          else if (value > nodeValue) balance(color, left, nodeValue, ins(right))
          else tree
      }
    }

    def balance(color: Color, left: Tree[A], value: A, right: Tree[A]): Tree[A] = {
      (color, left, value, right) match {
        case (Black, Node(Red, Node(Red, lllChild, llValue, llrChild), lValue, lrChild), value, right) =>
          Node(Red, Node(Black, lllChild, llValue, llrChild), lValue, Node(Black, lrChild, value, right))
        case (Black, Node(Red, llChild, lValue, Node(Red, lrlChild, lrValue, lrrChild)), value, right) =>
          Node(Red, Node(Black, llChild, lValue, lrlChild), lrValue, Node(Black, lrrChild, value, right))
        case (Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) => Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case (Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) => Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        case (c, l, v, r) => Node(c, l, v, r)
        case _ => throw new NoSuchElementException
      }
    }

    def blacken(tree: Tree[A]): Tree[A] = {
      tree match {
        case Node(_, left, nodeValue, right) => Node(Black, left, nodeValue, right)
      }
    }

    blacken(ins(tree))
  }

  @tailrec
  final def exist[A: Ordering](value: A, tree: Tree[A]): Boolean = {
    tree match {
      case Empty => false

      case Node(_, left, v, right) =>
        if (value < v) exist(value, left)
        else if (value > v) exist(value, right)
        else true
    }
  }

  final def getList[A: Ordering](tree: Tree[A]): List[A] = {
    @tailrec
    def summed(tree: Tree[A], acc: List[A]): List[A] = {
      tree match {
        case Node(_, Empty, v, Empty) => summed(Empty, v :: acc)
        case Node(_, Empty, v, r) => summed(r, v :: acc)
        case Node(_, Node(_, Empty, b, Empty), v, r) => summed(r, v :: b :: acc)
        // rebuild branch to a simpler problem
        case Node(_, Node(_, a, b, c), v, r) => summed(Node(Red, a, b, Node(Red, c, v, r)), acc)
        case Empty => acc
      }
    }

    summed(tree, List[A]())
  }

  final def fromList[A: Ordering](list: List[A]): Tree[A] = {
    @tailrec
    def innerFromList(list: List[A], res: Tree[A] = Empty): Tree[A] = {
      if (list.isEmpty)
        res
      else
        innerFromList(list.tail, insert(list.head, res))
    }

    innerFromList(list)
  }

  @tailrec
  final def union[A: Ordering](tree1: Tree[A])(tree2: Tree[A]): Tree[A] =
    tree1 match {
      case Empty => tree2
      case Node(_, _, v, _) => union(remove(v, tree1))(insert(v, tree2))
    }

  //lub
  //fromList(getList(tree1).union(getList(tree2)))

  final def intersect[A: Ordering](tree1: Tree[A], tree2: Tree[A]): Tree[A] = {
    @tailrec
    def innerIntersect(tree1: Tree[A], tree2: Tree[A])(res: Tree[A] = Empty): Tree[A] = {
      tree1 match {
        case Empty => res
        case Node(_, _, v, _) =>
          if (exist(v, tree2))
            innerIntersect(remove(v, tree1), tree2)(insert(v, res))
          else
            innerIntersect(remove(v, tree1), tree2)(res)
      }
    }

    innerIntersect(tree1, tree2)()
  }

  //lub
  //fromList(getList(tree1).intersect(getList(tree2)))

}