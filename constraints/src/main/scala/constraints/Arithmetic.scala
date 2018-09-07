package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
    * Transforms a positive integer in binary form into its integer representation.
    * The `head` element of the input list contains the most
    * significant bit (the list is in big-endian form).
    */
  def binary2int(n: List[Boolean]): Int = loop(n.reverse, 0, 1)

  @tailrec
  def loop(n: List[Boolean], acc: Int, i: Int): Int = n match {
    case false :: ns => loop(ns, acc, 2 * i)
    case true :: ns => loop(ns, i + acc, 2 * i)
    case Nil => acc
  }

  /**
    * Encodes a positive integer number into base 2.
    * The `head` element of the resulting list contains the most significant
    * bit. This function should not return unnecessary leading zeros.
    */
  def int2binary(n: Int): List[Boolean] = if (n == 0) List(false) else loop(n, List())

  @tailrec
  def loop(n: Int, acc: List[Boolean]): List[Boolean] =
    if (n > 0)
      loop(n / 2, (n % 2 == 1) :: acc)
    else
      acc

  /**
    * This function takes two arguments, both representing positive
    * integers encoded in binary as lists of propositional formulas.
    * It returns a list of both integers zipped (if lengths were different
    * then it adds '0' (or 'false') at their front to make them equal size)
    */
  def sameSize(n1: List[Formula], n2: List[Formula]): (List[Formula],List[Formula]) = {
    val f: Formula = false
    n1.reverse.zipAll(n2.reverse, f, f).reverse.unzip
  }

  /**
    * This function takes two arguments, both representing positive
    * integers encoded in binary as lists of propositional formulas
    * (true for 1, false for 0). It returns
    * a formula that represents a boolean circuit that constraints
    * `n1` to be less than or equal to `n2`
    */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    val (m1,m2) = sameSize(n1, n2)
    rec(m1,m2)
  }

  def rec(n1: List[Formula], n2: List[Formula]): Formula = {
    (n1, n2) match {
      case (f1:: Nil, f2::Nil)=> !f1 || f2
      case (f1::m1, f2::m2) => (!f1 || f2) && ( !((f1 && f2)||(!f1 && !f2)) || rec(m1,m2) )
    }
  }

  /**
    * A full adder is a circuit that takes 3 one bit numbers, and returns the
    * result encoded over two bits: (cOut, s)
    */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = {
    val cOut: Formula = (a && b) || (a && cIn) || (b && cIn)
    val s: Formula = (a xor b) xor cIn
    (cOut, s)
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
    val (m1,m2) = sameSize(n1,n2)
    recAdder(m1, m2)
  }
  def recAdder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
    (n1, n2) match {
      case (x :: Nil, y :: Nil) =>
        val (head, last) = fullAdder(x, y, false)
        val (newHead, newLast) = (propVar("newHead"), propVar("newSecond"))
        val constraints = (newHead iff head) && (newLast iff last)
        (newHead :: newLast :: Nil, Set(constraints))

      case (x :: xs, y :: ys) =>
        val (z::zs, oldSet) = recAdder(xs, ys)
        val (head, second) = fullAdder(x, y, z)
        val (newHead, newSecond) = (propVar("newHead"), propVar("newSecond"))
        val constraints = (newHead iff head) && (newSecond iff second)
        ( newHead :: newSecond :: zs, oldSet + constraints  )
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
