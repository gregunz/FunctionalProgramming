import java.util.Scanner

object p4 extends App {
  val std: Scanner = new Scanner(System.in)
  val length: Int = std.nextLine().toInt
  val exp: List[Int]= std.nextLine().map{_.toInt}.toList

  val head = exp.head

  def getDif (l: List[Int]): List[Int] = l match {
    case l1 :: Nil => Nil
    case l1 :: l2 :: ls => l1-l2 :: getDif(l2::ls)
  }

  val difs = head :: getDif(exp)
  val result = difs.min % 2 == 0

  println(if(result) "First" else "Second")

}