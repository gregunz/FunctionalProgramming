/**
  * Created by Gregoire on 01-Dec-15.
  */

import java.util.Scanner

object p2 extends App{
  val std: Scanner = new Scanner(System.in)
  val nk = std.nextLine().split(' ')
  val n:Int = nk(0).toInt
  val k = nk(1).toInt
  val l:List[Int] = std.nextLine().split(' ').map(_.toInt).sorted.toList
  val e = n-k+1
  val l1:List[(Int,Int)] = l.take(e).zipWithIndex
  val l2 = l.reverse.take(e).zipWithIndex
  val lowest:Int =  l1.head._1
  val highest = l2.head._1
  val onlyL1 = l1(e-1)._1 - lowest
  val onlyL2 = highest - l2(e-1)._1

  val max =for{
    (e1, i1) <- l1
    (e2, i2) <- l2
    if i1+i2 == e-1
  }yield (e1-lowest + highest-e2,(i1,i2))

  val (p1, p2) = max.find(_._1 == max.unzip._1.max).get._2


  print( l.slice(p1,n-p2) mkString " ")

}