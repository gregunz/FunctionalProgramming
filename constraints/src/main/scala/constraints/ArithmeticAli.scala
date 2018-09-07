package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._
import math._

import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object ArithmeticAli {

    /**
      * 1 for true, 0 for false
      */
    def booleanToInt(b: Boolean): Int = {
        if (b)
            1
        else
            0
    }

    /**
      * Transforms a positive integer in binary form into its integer representation.
      * The `head` element of the input list contains the most
      * significant bit (the list is in big-endian form).
      */
    def binary2int(n: List[Boolean]): Int = {
        def loop(list: List[Boolean], l: Int): Double = {
            if (list.nonEmpty)
                booleanToInt(list.head) * pow(2, l) + loop(list.tail, l - 1)
            else
                0
        }

        loop(n, n.length - 1).toInt
    }

    /**
      * Encodes a positive integer number into base 2.
      * The `head` element of the resulting list contains the most significant
      * bit. This function should not return unnecessary leading zeros.
      */
    def int2binary(n: Int): List[Boolean] = {
        n.toBinaryString.map{
            case '0' => false
            case '1' => true
        }.toList
    }

    def int2binaryComplexe(n: Int): List[Boolean] = {
        def loop(n: Int, power: Int): List[Boolean] = {
            if (n == 0)
                List(false)
            else if (n == 1)
                List(true)
            else {
                val twoToPower = pow(2, power)
                if (n < twoToPower)
                    false :: loop(n, power - 1)
                else
                    true :: loop(n - pow(2, power).toInt, power - 1)
            }
        }

        if (n < 0)
            throw new UnsupportedOperationException("Cannot convert negative integers")
        else {
            val power = floor(log(n) / log(2)).toInt
            loop(n, power)
        }
    }


    /**
      * This function takes two arguments, both representing positive
      * integers encoded in binary as lists of propositional formulas
      * (true for 1, false for 0). It returns
      * a formula that represents a boolean circuit that constraints
      * `n1` to be less than or equal to `n2`
      */
    def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
        def loop(n1: List[Formula], n2: List[Formula]): Formula = {
            if(n1.isEmpty) {
                true
            }
            else {
                !n1.head && n2.head || (n1.head == n2.head && lessEquals(n1.tail, n2.tail))
            }
        }

        if(n1 == n2)
            true
        else {
            val (nn1, nn2) = makeSameLength[Formula](false)(n1, n2)
            loop(nn1, nn2)
        }
    }

    /**
      * A full adder is a circuit that takes 3 one bit numbers, and returns the
      * result encoded over two bits: (cOut, s)
      */
    /*def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = {
        val axbxc, aAndB, axbAndC = propVar()
        val axb = a xor b
        val r = axb iff (a xor b)
        //(a && b) || (a && cIn) || (b && cIn)
        //(a && !b && !cIn) || (!a && b && !cIn) || (!a && !b && cIn) || (a && b && cIn)
        (
            axbAndC iff (axb && cIn) || (aAndB iff (a && b)),
            axb && axbxc iff (axb xor cIn)
        )
    }*/
    def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = {
        //(a && !b && !cIn) || (!a && b && !cIn) || (!a && !b && cIn) || (a && b && cIn)
        ((a && b) || (a && cIn) || (b && cIn), a xor b xor cIn)
    }

    def fullAdder(a: Formula, b: Formula): (Formula, Formula) = {
        (a && b, a xor b)
    }

    /**
      * This function takes two arguments, both representing positive integers
      * encoded as lists of propositional variables. It returns a pair.
      *
      * The first element of the pair is a `List[Formula]`, and it represents
      * the resulting binary number.
      * The second element is a set of intermediate constraints that are created
      * along the way.
      *
      */
    def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
        val (nn1, nn2) = makeSameLength[Formula](false)(n1, n2)

        def loop(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
            if(n1.isEmpty)
                (Nil, Set.empty)
            else{
                val (recurs, set) = loop(n1.tail, n2.tail)
                val ((cOut, r), rtail) = recurs match {
                    case x :: xs => ( fullAdder(n1.head, n2.head, x), xs)
                    case Nil => (fullAdder(n1.head, n2.head), Nil)
                }

                val cvar = propVar()
                val rvar = propVar()
                (cvar :: rvar :: rtail, set + (cvar iff cOut) + (rvar iff r))
            }
        }

        loop(nn1, nn2)
    }

    def adder2(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
        val (nn1, nn2) = (n1.reverse, n2.reverse)

        def loop(n1: List[Formula], n2: List[Formula], cIn: Formula, n: Int): (List[Formula], Set[Formula]) = {
            if(n1.isEmpty){
                if(n2.isEmpty){
                    (cIn :: Nil, Set.empty)
                }
                else{
                    val (cOut, r) = fullAdder(n2.head, cIn)
                    val (propC, propR) = (propVar("ca" + n) iff cOut, propVar("rs" + n) iff r)
                    val (rtail, set) = loop(n1, n2.tail, propC, n+1)
                    (propR :: rtail,  set + propC)
                }
            }
            else if(n2.isEmpty){
                val (cOut, r) = fullAdder(n1.head, cIn)
                val (propC, propR) = (propVar("ca" + n) iff cOut, propVar("rs" + n) iff r)
                val (rtail, set) = loop(n1.tail, n2, propC, n+1)
                (propR :: rtail,  set + propC)
            }
            else{
                val (cOut, r) = fullAdder(n1.head, n2.head, cIn)
                val (propC, propR) = (propVar("ca" + n) iff cOut, propVar("rs" + n) iff r)
                val (rtail, set) = loop(n1.tail, n2.tail, propC, n+1)
                (propR :: rtail,  set + propC)
            }
        }

        val (r, s) = loop(nn1, nn2, false, 1)
        (r.reverse, s)
    }

    def makeSameLength[T](nullElement: T)(a: List[T], b: List[T]): (List[T], List[T]) = {
        val al = a.length
        val bl = b.length

        if(al < bl){
            (List.fill(bl - al)(nullElement) ++ a, b)
        }
        else if(bl < al){
            (a, List.fill(al - bl)(nullElement) ++ b)
        }
        else{
            (a, b)
        }
    }

    /**
      * A helper function that creates a less-equals formula
      * taking an integer and a formula as parameters
      */
    def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
        lessEquals(int2binary(cst), n)
    }

    /**
      * A helper function that creates a less-equals formula
      * taking a formula and an integer as parameters
      */
    def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
        lessEquals(n, int2binary(cst))
    }


}
