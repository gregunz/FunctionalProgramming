def isSorted (ls : List[Int]): Boolean ={
  ls == ls.sorted
}

val l = List(3,4,0,7,1,2)
val l2 = List(1,2,4,6,8)
l.sorted

isSorted(l)
isSorted(l.sorted)
isSorted(l2)
isSorted(List())