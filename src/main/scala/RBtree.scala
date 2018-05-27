import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

/**
  * Colors for RBTree nodes
  */
sealed trait Color

case object Red extends Color

case object Black extends Color

/**
  * Abstract tree class used for Node and Empty case classes
  *
  * @tparam A Type of the value contained in RBtree
  */
sealed abstract class Tree[+A]()

/**
  * Case class for representing not empty nodes of the RBTree
  *
  * @param color Color of the Node
  * @param left  Left subtree
  * @param value Value of the node
  * @param right Right subtree
  */
case class Node[+A](color: Color, left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

/**
  * Case class for representing empty leaves of the RBTree
  */
case object Empty extends Tree[Nothing]


object Node {
  /**
    * Function for inserting values into tree.
    *
    * @param value Value which is to be inserted
    * @param tree  Tree in which we insert
    * @tparam A Type of value
    * @return New tree with inserted value
    */
  def insert[A: Ordering](value: A, tree: Tree[A]): Tree[A] = {
    /**
      * Function chosing in which subtree to insert value
      *
      * @param tree Tree in which we insert
      */
    def ins(tree: Tree[A]): Tree[A] = {
      tree match {
        case Empty => Node(Red, Empty, value, Empty)

        case Node(color, left, nodeValue, right) =>
          if (value < nodeValue) balance(color, ins(left), nodeValue, right)
          else if (value > nodeValue) balance(color, left, nodeValue, ins(right))
          else tree
      }
    }

    /**
      * Function for balancing tree after inserting
      *
      * @param color Color of node we want to balance
      * @param left  Left subtree
      * @param value Value of node
      * @param right Right subtree
      */
    def balance(color: Color, left: Tree[A], value: A, right: Tree[A]): Tree[A] = {
      (color, left, value, right) match {
        case (Black, Node(Red, Node(Red, lllChild, llValue, llrChild), lValue, lrChild), value, right) =>
          Node(Red, Node(Black, lllChild, llValue, llrChild), lValue, Node(Black, lrChild, value, right))
        case (Black, Node(Red, llChild, lValue, Node(Red, lrlChild, lrValue, lrrChild)), value, right) =>
          Node(Red, Node(Black, llChild, lValue, lrlChild), lrValue, Node(Black, lrrChild, value, right))
        case (Black, left, value, Node(Red, Node(Red, rllChild, rrValue, rlrChild), rValue, rrChild)) =>
          Node(Red, Node(Black, left, value, rllChild), rrValue, Node(Black, rlrChild, rValue, rrChild))
        case (Black, left, value, Node(Red, rlChild, rValue, Node(Red, rrlChild, rrValue, rrrChild))) =>
          Node(Red, Node(Black, left, value, rlChild), rValue, Node(Black, rrlChild, rrValue, rrrChild))
        case (color, left, value, right) => Node(color, left, value, right)
        case _ => throw new NoSuchElementException
      }
    }

    /**
      * FUnction for changing root to black
      *
      * @param tree Tree which root we want to recolor
      * @return Tree with root recolored to black
      */
    def blacken(tree: Tree[A]): Tree[A] = {
      tree match {
        case Node(_, left, nodeValue, right) => Node(Black, left, nodeValue, right)
        case _ => Empty
      }
    }

    blacken(ins(tree))
  }

  /**
    * Function for removing element from a tree
    *
    * @param value Value to be removed
    * @param tree  Tree from which we remove
    * @tparam A Type of value
    * @return Tree without removed value
    */
  final def remove[A: Ordering](value: A, tree: Tree[A]): Tree[A] = {
    /**
      * Balance after removal
      *
      * @param left  left subtree
      * @param value value
      * @param right right subtree
      * @return
      */
    def balance(left: Tree[A], value: A, right: Tree[A]): Tree[A] = (left, right) match {
      case (Node(Red, ll, lv, lr), Node(Red, rl, rv, rr)) =>
        Node(Red, Node(Black, ll, lv, lr), value, Node(Black, rl, rv, rr))
      case (Node(Red, Node(Red, lll, llv, llr), lv, lr), r) =>
        Node(Red, Node(Black, lll, llv, llr), lv, Node(Black, lr, value, r))
      case (Node(Red, ll, lv, Node(Red, lrl, lrv, lrr)), r) =>
        Node(Red, Node(Black, ll, lv, lrl), lrv, Node(Black, lrr, value, r))
      case (l, Node(Red, rl, rv, Node(Red, rrl, rrv, rrr))) =>
        Node(Red, Node(Black, l, value, rl), rv, Node(Black, rrl, rrv, rrr))
      case (l, Node(Red, Node(Red, rll, rlv, rlr), rv, rr)) =>
        Node(Red, Node(Black, l, value, rll), rlv, Node(Black, rlr, rv, rr))
      case (l, r) =>
        Node(Black, l, value, r)
    }

    /**
      * Balancing if we remove from left subtree
      */
    def balanceLeft(left: Tree[A], v: A, right: Tree[A]): Tree[A] = (left, right) match {
      case (Node(Red, ll, lv, lr), r) =>
        Node(Red, Node(Black, ll, lv, lr), v, r)
      case (l, Node(Black, rl, rv, rr)) =>
        balance(l, v, Node(Red, rl, rv, rr))
      case (l, Node(Red, Node(Black, rll, rlv, rlr), rv, rr)) =>
        Node(Red, Node(Black, l, v, rll), rlv, balance(rlr, rv, reden(rr)))
      case (Empty, Empty) => Node(Red, Empty, v, Empty)
      case _ => throw new Error("Something went wrong")
    }

    /**
      * Balancing if we remove from right subtree
      */
    def balanceRight(left: Tree[A], v: A, right: Tree[A]): Tree[A] = (left, right) match {
      case (l, Node(Red, rl, rv, rr)) =>
        Node(Red, l, v, Node(Black, rl, rv, rr))
      case (Node(Black, ll, lv, lr), r) =>
        balance(Node(Red, ll, lv, lr), v, r)
      case (Node(Red, ll, lv, Node(Black, lrl, lrv, lrr)), r) =>
        Node(Red, balance(reden(ll), lv, lrl), lrv, Node(Black, lrr, v, r))
      case (Empty, Empty) => Node(Red, Empty, v, Empty)
      case _ => throw new Error("Something went wrong")
    }

    def removeLeft(c: Color, l: Tree[A], v: A, r: Tree[A], remVal: A): Tree[A] = {
      (c, l, v, r) match {
        case (Black, _, _, _) => balanceLeft(rem(remVal, l), v, r)
        case (Red, _, _, _) => Node(Red, rem(remVal, l), v, r)
      }
    }

    def removeRight(c: Color, l: Tree[A], v: A, r: Tree[A], remVal: A): Tree[A] = {
      (c, l, v, r) match {
        case (Black, _, _, _) => balanceRight(l, v, rem(remVal, r))
        case (Red, _, _, _) => Node(Red, l, v, rem(remVal, r))
      }
    }

    /**
      * Function for making one tree from two children of removed node
      *
      * @param left  Left child of removed node
      * @param right Right child of removed node
      */
    def add(left: Tree[A], right: Tree[A]): Tree[A] = (left, right) match {
      case (Empty, r) => r
      case (l, Empty) => l
      case (Node(Red, ll, lv, lr), Node(Red, rl, rv, rr)) =>
        add(lr, rl) match {
          case Node(Red, a, va, b) => Node(Red, Node(Red, ll, lv, a), va, Node(Red, b, rv, rr))
          case c => Node(Red, ll, lv, Node(Red, c, rv, rr))
        }
      case (Node(Black, ll, xv, lr), Node(Black, rl, rv, rr)) =>
        add(lr, rl) match {
          case Node(Red, a, va, b) => Node(Red, Node(Red, ll, xv, a), va, Node(Black, b, rv, rr))
          case c => balanceLeft(ll, xv, Node(Black, c, rv, rr))
        }
      case (l, Node(Red, rl, rv, rr)) => Node(Red, add(l, rl), rv, rr)
      case (Node(Red, ll, lv, lr), r) => Node(Red, ll, lv, add(lr, r))
      case _ => throw new Error("Something went wrong")
    }

    /**
      * Function for recoloring node to black
      */
    def blacken(tree: Tree[A]): Tree[A] = {
      tree match {
        case Node(_, left, nodeValue, right) => Node(Black, left, nodeValue, right)
        case _ => Empty
      }
    }

    /**
      * Function for recoloring node to red
      */
    def reden(tree: Tree[A]): Tree[A] = tree match {
      case Node(Black, l, v, r) => Node(Red, l, v, r)
      case _ => throw new Error("Expected black node in reden function")
    }

    def rem(value: A, tree: Tree[A]): Tree[A] = {
      val newTree: Tree[A] = tree match {
        case Empty => Empty
        case Node(c, l, v, r) => {
          if (value < v) removeLeft(c, l, v, r, value)
          else if (v < value) removeRight(c, l, v, r, value)
          else add(l, r)
        }
      }
      newTree
    }

    blacken(rem(value, tree))
  }

  /**
    * Function for checking if value exists in tree
    *
    * @param value Value which existance we want to check
    * @param tree  Tree in which we look for it
    * @tparam A Type of Value
    * @return Result of search
    */
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

  /**
    * Function for creating an union of 2 trees
    *
    * @param tree1 First tree
    * @param tree2 Second tree
    * @tparam A Type of value stored inside tree
    * @return New tree which is union of 2 trees
    */
  @tailrec
  final def union[A: Ordering](tree1: Tree[A])(tree2: Tree[A]): Tree[A] =
    tree1 match {
      case Empty => tree2
      case Node(_, _, v, _) => union(remove(v, tree1))(insert(v, tree2))
    }

  /**
    * Function for intersecting 2 tree
    *
    * @param tree1 First tree
    * @param tree2 Second tree
    * @tparam A Type of value
    * @return New tree which is intersection of 2 trees
    */
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
}