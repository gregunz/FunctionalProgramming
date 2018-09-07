/**
  * Created by Gregoire on 01-Dec-15.
  */
object test extends App {
 def a = {println("a");1}
 def b = {println("b");1}

  val list : List[Int] = List(a, a+1, a, a, a).filter(i => i==1).map{ i => a}
  val stream: Stream[Int] = Stream (a, a, a ,a ,a)

}
