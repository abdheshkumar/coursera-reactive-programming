package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //  If you insert an element into an empty heap, then delete the minimum, the resulting
  // heap should be empty.
  property("insert delete") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  //  If you insert any two elements into an empty heap, finding the minimum of the
  // resulting heap should get the smallest of the two elements back.
  property("insert 2") = forAll { (a: Int, b: Int) =>
    val hab = insert(b, insert(a, empty))
    val hba = insert(a, insert(b, empty))
    if (a < b) {
      findMin(hab) == a && findMin(hba) == a
    } else {
      findMin(hab) == b && findMin(hba) == b
    }
  }

  //  Given any heap, you should get a sorted sequence of elements when continually finding
  // and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sorted seq") = forAll { (h: H) =>
    def pop(h: H, last: Int): Boolean = {
      val min = findMin(h)
      val h2 = deleteMin(h)
      last <= min && (isEmpty(h2) || pop(h2, min))
    }
    isEmpty(h) || pop(h, Int.MinValue)
  }

  //  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld min") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val min = findMin(h)
    min == findMin(h1) || min == findMin(h2)
  }

  property("insert list and check") = forAll { l: List[Int] =>
    def pop(h: H, l: List[Int]): Boolean = {
      if (isEmpty(h)) {
        l.isEmpty
      } else {
        !l.isEmpty && findMin(h) == l.head && pop(deleteMin(h), l.tail)
      }
    }
    val sl = l.sorted
    val h = l.foldLeft(empty)((he, a) => insert(a, he))
    pop(h, sl)
  }
}
