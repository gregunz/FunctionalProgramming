import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

val cs : List[Char]= List('a','b','c')
val is : List[Int]= List(1,2,3,4)
val l : List[(Char, Int)] = (cs.reverse zipAll (is.reverse, '.', 0)).reverse.toList

val f : Formula = false || false

solveForSatisfiability(f)

val l1 = List(1,2,3,4)
val l2 = List(6,7,8,9)

l1.drop(1) zip l2.take(3)

def createPairs[T](l1 : List[T], l2: List[T]): (List[(T, T)],List[(T, T)])= {
  val all = for {
    a <- l1
    b <- l2
  } yield (a,b)
  val neg = for {
    a <- l1.drop(1)
    b <- l2.take(l1.indexOf(a))
  } yield (a,b)
  (neg , (all.toSet -- neg.toSet).toList)
}

createPairs(l1,l2)
def sameSizeZipped(n1: List[Boolean], n2: List[Boolean]): List[(Boolean, Boolean)] = {
  val f = false
  n1.reverse.zipAll(n2.reverse, f, f).reverse
}


val f1 = List(propVar("a"), propVar("b"))
val f2 = List(propVar("x"), propVar("y"))

val b1 =List(false,true,true)
val b2 = List(true, true)

val (b11, b22) = sameSizeZipped(b1,b2).unzip
b1.flatMap(f => List(!f, f))
solveForSatisfiability(true iff propVar("a"))
propVar() iff propVar()
def rec(n1: List[Boolean], n2: List[Boolean]): Boolean = {
  (n1, n2) match {
    case (f1:: Nil, f2::Nil)=> !f1 || f2
    case (f1::m1, f2::m2) => (!f1 || f2) && ( !((f1 && f2)||(!f1 && !f2)) || rec(m1,m2) )
  }
}

rec(b11,b22)
rec(b22,b11)

