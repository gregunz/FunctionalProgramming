package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val emptyHeap : Gen[H] = const(empty)

  lazy val genHeap: Gen[H] = oneOf(emptyHeap, oneOrMore)

  lazy val oneOrMore: Gen[H] = for{
    i <- arbitrary[Int]
    h <- oneOf(emptyHeap, oneOrMore)
  } yield insert(i,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (i1: Int, i2: Int) =>
    val m = if (i1 < i2) i1 else i2
    findMin(insert(i2, insert(i1, empty))) == m
  }

  property("gen3") = forAll { (i: Int) =>
    deleteMin(insert(i, empty)) == empty
  }

  def getList(h: H): List[Int] = {
    if(isEmpty(h))
      Nil
    else
      findMin(h) :: getList(deleteMin(h))
  }
  def isSorted (ls : List[Int]): Boolean ={
    ls == ls.sorted
  }

  property("gen4") = forAll { (h: H) =>
    val ls = getList(insert(Integer.MAX_VALUE,h))
    isSorted(ls) && ls.last == Integer.MAX_VALUE
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    val minMelded = if(isEmpty(melded)) Integer.MAX_VALUE else findMin(melded)
    val minH1 = if(!isEmpty(h1)) findMin(h1) else Integer.MAX_VALUE
    val minH2 = if(!isEmpty(h2)) findMin(h2) else Integer.MAX_VALUE
    val m = if(minH1 < minH2) minH1 else minH2
    minMelded == m
  }
/*
  property("gen6") = forAll { (i1: Int, i2: Int) =>
    val max = if (i1 > i2) i1 else i2
    findMin(deleteMin(insert(i2, insert(i1, empty)))) == max
  }

  def getMax(h: H, lastVal: Int): Int ={
    if(isEmpty(h))
      lastVal
    else
      getMax(deleteMin(h), findMin(h))
  }

  property("gen7") = forAll { (h: H) =>
    getMax(insert(Integer.MAX_VALUE, h), Integer.MIN_VALUE) == Integer.MAX_VALUE
  }
*/
}
