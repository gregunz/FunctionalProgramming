package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

/**
  * This component implements a constraint solver
  * for assigning time slots to bands at a festival
  */
object ConcertsPlanner {

  case class Band(name: String) {
    override def toString = name
  }

  case class Stage(name: String) {
    override def toString = name
  }

  case class Time(time: String) {
    override def toString = time
  }

  type Slot = (Stage, Time)

  /**
    * This function schedules bands to slots. It takes as input
    * a list of preferences, as a map from bands to the list of
    * slots the band wishes to play in.
    *
    * The result is an `Option[Map[Band, Slot]]`. The function attemps to
    * assign a unique and different slot to each band. It returns None if
    * no complete valid scheduling exists.
    * If the problem has a complete valid assignment, a map from every
    * band to a slot is returned.
    * No partial solution is returned, if only some of the band can be assigned
    * a slot, then None is returned.
    */

  def plan(preferences: Map[Band, List[Slot]]): Option[Map[Band, Slot]] = {

    val bands: Seq[Band] = preferences.keys.toSeq
    val slots: Seq[Slot] = getUniqueSlots(preferences).toSeq

    //generates one propositional variable per band/slot combination
    val varsMatrix: Map[(Band, Slot), PropVar] =
      bands.flatMap{ case b@Band(name) =>
        slots.map(s => (b, s) -> propVar(name))
      }.toMap

    //Set of constraints ensuring each band gets a desired slot
    val desirableSlots: Seq[Formula] =
      bands.map { b =>
        or(slots.filter(s => preferences(b).contains(s)).map {
          s => varsMatrix((b, s))
        }: _*)
      }

    def createPairs[T] (l : Seq[T]): Seq[(T, T)] ={
      for{
        (e1, i) <- l.zipWithIndex
        e2 <- l.drop(i+1)
      }yield (e1, e2)
    }
    //A set of constraints ensuring that a band gets at most one slot
    val eachBandPlaysOnce: Seq[Formula] = {
      createPairs(bands).map{
        case(b1, b2) =>
          and(slots.map(s => !varsMatrix(b1,s) || !varsMatrix(b2,s)):_*)
      }
    }

    //A set of constraints ensuring that each slot is used at most once
    val eachSlotUsedOnce: Seq[Formula] = {
      createPairs(slots).map{
        case(s1, s2) =>
          and(bands.map(b => !varsMatrix(b,s1) || !varsMatrix(b,s2)):_*)
      }
    }


    //combining all the constraints together
    val allConstraints: Seq[Formula] =
      desirableSlots ++ eachBandPlaysOnce ++ eachSlotUsedOnce

    //finding a satisfying assignment to the constraints
    val res = solveForSatisfiability(and(allConstraints:_*))
    /*
    println(bands mkString " ; ")
    println(slots mkString " ; ")
    println(varsMatrix mkString " ; ")
    println(allConstraints mkString "and")
    */
    val test  = res.map(model => {
      //println(model)
      bands.map(band => {
        //print(band+"+")
        val assignedSlot = slots.find(slot => model(varsMatrix((band, slot))))
        //print(assignedSlot+"\n")
        (band, assignedSlot.get)
      }).toMap
    })
    //println("--"+test)
    test
  }

  /**
    * This function takes a preference map, and returns all unique slots that are
    * part of the preferences.
    */

  def getUniqueSlots(preferences: Map[Band, List[Slot]]): Set[Slot] = {
    //preferences.values.flatten.toSet
    preferences.map(_._2).flatten.toSet
  }

}
