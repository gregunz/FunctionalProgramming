package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._

import ch.epfl.lamp.grading.GradingSuite
import ch.epfl.lamp.grading.instrumented.InstrumentedSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@RunWith(classOf[JUnitRunner])
class ArithmeticSuite extends FunSuite with ScalaFutures {

  import Arithmetic._

  test("binary2int works with one bit") {
    assert(binary2int(List(true)) === 1)
  }

  test("binary2int of one false is 0") {
    assert(binary2int(List(false)) === 0)
  }

  test("binary2int ignores leading zeros") {
    assert(binary2int(List(false, true)) === 1)
    assert(binary2int(List(false, false, true)) === 1)
    assert(binary2int(List(false, false, false)) === 0)
    assert(binary2int(List(false, true, false)) === 2)
    assert(binary2int(List(false, true, true)) === 3)
  }



  test("int2binary works on one bit numbers") {
    assert(int2binary(1) === List(true))
  }

  test("int2binary of 0 is false") {
    assert(int2binary(0) === List(false))
  }

  test("int2binary works with numbers needing 2 bits") {
    assert(int2binary(2) === (List(true, false)))
    assert(int2binary(3) === (List(true, true)))
  }

  test("binary2int of int2binary "){
    assert(binary2int(int2binary(55)) === 55)
    assert(binary2int(int2binary(99)) === 99)
    assert(!(binary2int(int2binary(98)) === 99))
    assert(!(binary2int(int2binary(56)) === 55))
  }


  test("lessEquals between constant numbers of one bit") {
    val na1: List[Formula] = List(false)
    val na2: List[Formula] = List(false)
    val ra = lessEquals(na1, na2)

    val nb1: List[Formula] = List(false)
    val nb2: List[Formula] = List(true)
    val rb = lessEquals(nb1, nb2)

    val nc1: List[Formula] = List(true)
    val nc2: List[Formula] = List(true)
    val rc = lessEquals(nc1, nc2)

    val nd1: List[Formula] = List(true)
    val nd2: List[Formula] = List(false)
    val rd = lessEquals(nd1, nd2)
    //println(List(ra,rb,rc,rd) mkString "\n")
    assert(solveForSatisfiability(ra) != None)
    assert(solveForSatisfiability(rb) != None)
    assert(solveForSatisfiability(rc) != None)
    assert(solveForSatisfiability(rd) === None)
  }

  test("lessEquals between constants is working") {
    val na1: List[Formula] = List(true, false, true)
    val na2: List[Formula] = List(true, true, true)
    val ra = lessEquals(na1, na2)

    val nb1: List[Formula] = List(true, false, true)
    val nb2: List[Formula] = List(false, true, true)
    val rb = lessEquals(nb1, nb2)

    val nc1: List[Formula] = List(true, false, true)
    val nc2: List[Formula] = List(true, false, false)
    val rc1 = lessEquals(nc1, nc2)
    val rc2 = lessEqualsConst(nc1, 3)

    val nd1: List[Formula] = List(true, false, true)
    val nd2: List[Formula] = List(true, true, false)
    val rd = lessEquals(nd1, nd2)
    //println(List(ra,rb,rc1,rc2,rd) mkString "\n")
    //println(solveForSatisfiability(ra))
    assert(!solveForSatisfiability(ra).isEmpty)
    assert(solveForSatisfiability(rb) === None)
    assert(solveForSatisfiability(rc1) === None)
    assert(solveForSatisfiability(rc2) === None)
    assert(solveForSatisfiability(rd) != None)
  }



  test("fullAdder is implemented") {
    fullAdder(true, true, true)
  }

  test("fullAdder is working with constants") {
    def isSat(f: Formula): Boolean = solveForSatisfiability(f) != None
    
    val (c1, s1) = fullAdder(false, false, false)
    assert(!isSat(c1))
    assert(!isSat(s1))

    val (c2, s2) = fullAdder(false, true, false)
    assert(!isSat(c2))
    assert(isSat(s2))

    val (c3, s3) = fullAdder(true, false, false)
    assert(!isSat(c3))
    assert(isSat(s3))

    val (c4, s4) = fullAdder(true, true, false)
    assert(isSat(c4))
    assert(!isSat(s4))

    val (c5, s5) = fullAdder(true, false, true)
    assert(isSat(c5))
    assert(!isSat(s5))

    val (c6, s6) = fullAdder(true, true, true)
    assert(isSat(c6))
    assert(isSat(s6))
  }

  test("fullAdder is working with symbolic variables") {
    val a = propVar()
    val b = propVar()
    val cIn = propVar()

    val (cOut, s) = fullAdder(a, b, cIn)

    assert(solveForSatisfiability(cOut && a && b && !cIn) != None)
    assert(solveForSatisfiability(s && a && b && cIn) != None)
  }


  test("adder of one bit numbers") {
    val na1: List[Formula] = List(true)
    val na2: List[Formula] = List(true)
    val (ra, ca) = adder(na1, na2)
    //println(ra,"\n",ca)
    assert(checkNumber(ra, ca, List(true, false)))
    assert(!checkNumber(ra, ca, List(true, true)))
    assert(!checkNumber(ra, ca, List(false, true)))
    assert(!checkNumber(ra, ca, List(true)))
    assert(!checkNumber(ra, ca, List(false)))

    val nb1: List[Formula] = List(true)
    val nb2: List[Formula] = List(false)
    val (rb, cb) = adder(nb1, nb2)
    assert(checkNumber(rb, cb, List(true)))

    val nc1: List[Formula] = List(false)
    val nc2: List[Formula] = List(true)
    val (rc, cc) = adder(nc1, nc2)
    assert(checkNumber(rc, cc, List(true)))

    val nd1: List[Formula] = List(false)
    val nd2: List[Formula] = List(false)
    val (rd, cd) = adder(nd1, nd2)
    assert(checkNumber(rd, cd, List(false)))
    assert(!checkNumber(rd, cd, List(true)))

  }

  test("adder is computing correct result for constants") {
    val na1: List[Formula] = List(false, true)
    val na2: List[Formula] = List(true, true)
    val (ra, ca) = adder(na1, na2)
    //println(ra,"\n",ca)
    assert(checkNumber(ra, ca, List(true, false, false)))
    assert(!checkNumber(ra, ca, List(true, true, false, false)))
    assert(!checkNumber(ra, ca, List(false, false, false)))

    val nb1: List[Formula] = List(true, false, true)
    val nb2: List[Formula] = List(false, true, true)
    val (rb, cb) = adder(nb1, nb2)
    assert(checkNumber(rb, cb, List(true, false, false, false)))

    val nc1: List[Formula] = List(true, false, true)
    val nc2: List[Formula] = List(true, false, true)
    val (rc, cc) = adder(nc1, nc2)
    assert(checkNumber(rc, cc, List(true, false, true, false)))
    assert(!checkNumber(rc, cc, List(true, true, true, true)))

    val ne1: List[Formula] = List(true, true, true, true) //15
    val ne2: List[Formula] = List(true, true, false, true) //13
    val (re, ce) = adder(ne1, ne2)
    assert(checkNumber(re, ce, List(true,true,true,false,false))) //28
    assert(!checkNumber(re, ce, List(true,false,true,false,false))) //20

  }

  test("adder is working with numbers of different length") {
    val na1: List[Formula] = List(true)
    val na2: List[Formula] = List(true, true)
    val (ra, ca) = adder(na1, na2)
    assert(checkNumber(ra, ca, List(true, false, false)))

    val nb1: List[Formula] = List(true, false, true)
    val nb2: List[Formula] = List(true, true)
    val (rb, cb) = adder(nb1, nb2)
    assert(checkNumber(rb, cb, List(true, false, false, false)))

    val nc1: List[Formula] = List(true)
    val nc2: List[Formula] = List(true, false)
    val (rc, cc) = adder(nc1, nc2)
    assert(checkNumber(rc, cc, List(true, true)))
    assert(!checkNumber(rc, cc, List(true, false)))
  }

  test("adder formula size is polynomial") {
    val inputs: List[List[Formula]] = (1 to 3).toList.map(n => int2binary(n): List[Formula])

    var context: Set[Formula] = Set()
    val res1: List[Formula] = inputs.foldLeft[List[Formula]](List(false))((acc, el) => {
      val (nacc, cs) = adder(acc, el)
      context ++= cs
      nacc
    })
    //println(res1);println(context)
    assert((res1 ++ context).map(_.size).sum < 2000)

  }

  test("adder(F + cnst) and lessEquals") {
    val nb1: List[Formula] = List(true, false, true) //5
    val cnst = int2binary(3)
    val (sum, set) = adder(nb1,cnst)
    //val sum = and(set.toSeq)
    //val sum = int2binary(8)

    val huit = int2binary(8)
    val neuf = int2binary(9)

    val isTrue = lessEquals(huit, sum)
    val isFalse = lessEquals(neuf, sum)

    //assert(solveForSatisfiability(isTrue && and(set.toSeq:_*)) != None)
    //assert(solveForSatisfiability(isFalse && and(set.toSeq:_*)) == None)
    assert(checkNumber(sum, set, huit))

  }


  test("composition of adder to sum a list of numbers") {
    val inputs: List[List[Formula]] = List(1,2,3,4,5,6,7,8,9,10).map(n => int2binary(n): List[Formula])

    val result = Future {
      var context: Set[Formula] = Set()
      val res: List[Formula] = inputs.foldLeft[List[Formula]](List(false))((acc, el) => {
        val (nacc, cs) = adder(acc, el)
        context ++= cs
        nacc
      })

      assert(checkNumber(res, context, int2binary(55)))
      //assert(!checkNumber(res, context, int2binary(56)))
    }
    assert(result.isReadyWithin(2 seconds), "The operation took too much time")
  }



  private def checkNumber(res: List[Formula], context: Set[Formula], expected: List[Boolean]): Boolean = {

    val rres = res.reverse

    val digitsOk = and(rres.zip(expected.reverse).map(p => p._1 iff p._2):_*)
    val leadingZeros = !or(rres.drop(expected.size):_*)

    val theorem = digitsOk && leadingZeros && and(context.toSeq:_*)
    //println("size: ",expected.size <= res.size)
    //println(solveForSatisfiability(theorem))
    expected.size <= res.size && solveForSatisfiability(theorem) != None
  }

}
