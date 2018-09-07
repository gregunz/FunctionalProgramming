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

fp(6,3)

def length2[T](xs: List[T]):Int = xs.foldRight(0)((p, z) => z+1)

val a = List(2,4,5,6,4,3,44,4,34,423,3,22,3)

a.length

length2(a)