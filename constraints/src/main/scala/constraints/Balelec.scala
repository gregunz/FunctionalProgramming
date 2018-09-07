package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

/**
  * This component implements a constraint solver for assigning time slots to volunteers
  * for various tasks at a festival. A task may require more than one volunteer,
  * and a volunteer can take a limited number of tasks
  */
object Balelec {

  import Arithmetic._

  case class Volunteer(name: String) {
    override def toString = name
  }

  /**
    * A task is represented by its name and
    * its capacity, i.e. the exact number of people
    * required to complete it.
    */
  case class Task(name: String, capacity: Int) {
    override def toString = name
  }

  /**
    * This function schedules volunteers to tasks.
    * It takes as input a list of volunteers and a list of tasks.
    * The `availability` map contains mappings from volunteers to the
    * tasks they are available for.
    * A volunteer can be assigned to several tasks, but only
    * up to a maximum number of task specified by the `maxWorkload` parameter.
    * It is ok to not assign a volunteer to any task.
    *
    * The return value is a list of volunteers assigned to each task. The function only
    * returns a complete valid assignment, if no such assignment exists then the
    * function returns None.
    */
  def schedule(
                volunteers: List[Volunteer],
                tasks: List[Task],
                availability: Map[Volunteer, List[Task]],
                maxWorkload: Int
              ): Option[Map[Task, List[Volunteer]]] = {

    val varsMatrix: Map[(Volunteer, Task), PropVar] =
      (for{
        v <- volunteers
        t <- tasks
      } yield ((v, t) , propVar())).toMap

    val constraints1: Seq[Formula] =
      volunteers.map{ v =>
        positivesLessEquals(tasks.map{ t =>
          varsMatrix((v,t))
        }, maxWorkload)
      }

    val constraints2: Seq[Formula] =
      volunteers.map{ v =>
        val (yes, no) = tasks.partition(t => availability(v).contains(t))
        or(yes.map {
          t => varsMatrix((v, t))
        }: _*) &&
          and(no.map {
            t => !varsMatrix((v, t))
          }: _*)
      }

    val constraints3: Seq[Formula] =
      tasks.map{ t =>
        val (res, context) = countPositives(varsMatrix.filter{ case((_,tmpT),_) => tmpT == t}.values.toList)
        checkNumber(res, context, int2binary(t.capacity))
      }

    val allConstraints: Seq[Formula] = constraints1 ++ constraints2 ++ constraints3
    val oneCons : Formula = and(allConstraints:_*)

    solveForSatisfiability(oneCons) match {
      case None => None
      case Some(model) =>
        Some(
        tasks.map{ t =>
          (t, varsMatrix.filter{ case ((_,tmpT),p) => model(p) && tmpT == t}.map{ case((v,_),_) => v }.toList)
        }.toMap)
    }
  }

  def checkNumber(res: List[Formula], context: Set[Formula], expected: List[Boolean]): Formula = {
    val rres = res.reverse
    val digitsOk = and(rres.zip(expected.reverse).map(p => p._1 iff p._2):_*)
    val leadingZeros = !or(rres.drop(expected.size):_*)
    digitsOk && leadingZeros && and(context.toSeq:_*)
  }

  /**
    * This function takes a list of constraint, and returns a
    * constraint that is true if and only if at most max
    * of them are true.
    */
  def positivesLessEquals(ns: List[Formula], max: Int): Formula = {
    val (r, c) = countPositives(ns)
    lessEquals(r, int2binary(max)) && and(c.toSeq:_*)
  }

  /**
    * This function takes a list of constraints, and returns a pair.
    * The first element of the pair is a list of constraints representing the bitwise
    * sum of the constraints of `ns`.
    * The second element is a set of additional constraints that have been gathered along
    * the way. Hint: see `adder` for understanding how to use additional constraints
    */
  def countPositives(ns: List[Formula]): (List[Formula], Set[Formula]) = {
    ns.foldLeft((List[Formula](false), Set[Formula]())) { case ((tmpSum, tmpAcc), n) =>
      val (r, c) = adder(tmpSum, List(n))
      (r, tmpAcc ++ c)
    }
  }

}
