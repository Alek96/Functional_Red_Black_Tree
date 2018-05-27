import Node._
import org.scalatest.FunSuite

class RBtreeTest extends FunSuite {
  test("Two nodes are equal") {
    val n1 = Node(Black, Empty, 1, Empty)
    val n2 = Node(Black, Empty, 1, Empty)
    assert(n1 == n2)
  }

  test("Two nodes are not equal") {
    val n1 = Node(Black, Empty, 2, Empty)
    val n2 = Node(Black, Empty, 1, Empty)
    assert(n1 != n2)
  }

  test("Value is in tree after adding") {
    val tree1 = Empty
    val tree2 = insert(2, tree1)
    assert(exist(2, tree2))
  }

  test("Not inserted value is not in tree") {
    val tree = insert(1, insert(2, Empty))
    assert(!exist(3, tree))
  }

  test("Removed value is not in tree") {
    val tree = remove(1, insert(1, insert(2, Empty)))
    assert(!exist(1, tree))
  }

  test("Value is in tree after removing other value") {
    val tree = remove(1, insert(1, insert(2, insert(3, insert(4, insert(5, insert(6, Empty)))))))
    assert(exist(2, tree))
  }

  test("Removing from empty tree returns empty tree") {
    assert(remove(1, Empty) == Empty)
  }

  test("Removing only value gives empty tree") {
    val tree = insert(1, Empty)
    assert(remove(1, tree) == Empty)
  }

  test("Two tree are equal") {
    val tree1 = insert(1, insert(2, insert(3, Empty)))
    val tree2 = insert(1, insert(2, insert(3, Empty)))
    assert(tree1 == tree2)
  }

  test("Union contains all elements") {
    val tree1 = insert(1, insert(2, insert(3, Empty)))
    val tree2 = insert(4, insert(5, insert(6, Empty)))
    val bigTree = insert(3, insert(2, insert(1, insert(4, insert(5, insert(6, Empty))))))
    assert(union(tree1)(tree2) == bigTree)
  }

  test("Union of tree and Empty is tree") {
    val tree = insert(1, Empty)
    assert(union(tree)(Empty) == tree)
  }

  test("Union of 2 Empty is Empty") {
    assert(union(Empty)(Empty) == Empty)
  }

  test("Intersect contains proper values") {
    val tree1 = insert(1, insert(2, insert(3, Empty)))
    val tree2 = insert(2, insert(3, insert(4, Empty)))
    assert(intersect(tree1, tree2) == insert(3, insert(2, Empty)))
  }

  test("Intersect of tree and Empty is Empty") {
    val tree = insert(1, insert(2, Empty))
    assert(intersect(tree, Empty) == Empty)
  }

  test("Intersect of Empty and Empty is Empty") {
    assert(intersect(Empty, Empty) == Empty)
  }

  test("Root is black") {
    val tree = insert(1, Empty)
    val isBlack = tree match {
      case Node(Black, _, _, _) => true
      case _ => false
    }
    assert(isBlack)
  }

  test("Root after removal is black") {
    val tree = insert(2, insert(1, Empty))
    val tree2 = remove(1, tree)
    val isBlack = tree2 match {
      case Node(Black, _, _, _) => true
      case _ => false
    }
    assert(isBlack)
  }
}