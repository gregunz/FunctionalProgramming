case class Poly(ls: List[Int])
case class SparsePoly(repr: List[(Int, Int)])

val p1 = Poly(List(3,0,0,0,-5))
val p2 = toSparse(p1)
val p3 = toDense(p2)


def +(that: Poly): Poly = ???
def toSparse(p: Poly): SparsePoly = {
  SparsePoly(
    p.ls.zipWithIndex.filter(p => p._1 !=0)
  )
}




def toDense(s: SparsePoly): Poly = {
  val tot = s.repr.last._2
  val m = s.repr.map{case (x,y) => (y,x)}.toMap withDefaultValue 0
  Poly((for{i <- 0 to tot}yield {m(i)}).toList)
}

def foldRight[T, Z](xs: List[T], z:Z, f:(T,Z) => Z) : Z = xs match {
  case Nil => z
  case x :: xs => f(x, foldRight(xs, z, f))
}

def length[T](xs: List[T]) : Int = foldRight(xs, 0, (p:T, z:Int) => z+1)

length(p1.ls)
def ls : List[Any] = List(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)
length(ls)

val ls2 = List(1,-2,3,-5,6)
ls2.head
ls2.tail

def sumelem(ls : List[Int]): List[Int] = {
  (ls.tail.scan(ls.head)((i1: Int, i2: Int) => i2 - i1) zip 0 :: 0 :: ls.tail.scan(ls.head)((i1: Int, i2: Int) => i2 - i1).take(ls.length-2)
    ).map(e => e._1 - e._2)
}
def sumelem2(ls: List[Int]) : List[Int] = {
  ((0 :: ls) zip ls).map(e => e._2 - e._1)
}
sumelem(ls2)
sumelem2(ls2)




def antisumelem(ls : List[Int]) : List[Int] = {
  ls.scan(0)((i1: Int, i2: Int)=> i1 + i2) drop 1
}

antisumelem(sumelem(ls2))