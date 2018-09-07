import scala.collection.mutable

def fp(a:Long, n:Int):Long = {

  n match {
    case 0 => 1
    case 1 => a
    case _ =>
      if(isEven(n))
        fp(a*a, n/2)
      else
        a*fp(a*a, n/2)
  }
}

def isEven(n: Int) = n%2 == 0

fp(6,20)

val l1 = mutable.LinkedList('A','B','B','A')
l1.next



def isPal (l : mutable.LinkedList[Char]): Boolean = {
 val x = l1
  val l2 = mutable.LinkedList(x.head)
}

def loop(l: mutable.LinkedList[Char]){
  if(l.head == Nil)
    l
  else
    l.
}