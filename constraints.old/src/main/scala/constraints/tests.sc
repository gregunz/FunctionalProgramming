import cafesat.api._
import FormulaBuilder._
import Formulas._

val a: PropVar = propVar()
val b: PropVar = propVar()
val f: Formula = a && (!a || b)
val f2: Formula = a &&   b

def solve(f: Formula)= {
  Solver.solveForSatisfiability(f) match {
    case None => {
      println("There is no solution")
    }
    case Some(model) => {
      println("a is: " + model(a))
      println("b is: " + model(b))
    }
  }
}

solve(f)
solve(f2)
val l = List(1,2,3,4,5)
def fun[T] (l : List[T]): List[(T, T)] ={
  for{
    e1 <- l
    e2 <- l.drop(l.indexOf(e1)+1)
  }yield (e1, e2)

}

fun(l)
l.contains(0)
l.contains(2)
l.contains(6)