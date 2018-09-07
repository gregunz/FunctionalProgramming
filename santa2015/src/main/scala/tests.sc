def a = {print("a");1}
def b = {print("b");1}

def list =
  List(a, a+1, a, a, a)
    .zipWithIndex
    .filter{i => print("f");i._1==1}
    .map{ i => (b, i._2)}

def stream=
  Stream (b, b, b+1 ,b ,b)
    .zipWithIndex
    .filter{i => print("f");i._1==1}
    .map{ i => (a, i._2)}

def lazyy (x: => Int) = {
  for(i <- 1 to 10)yield x
  x
}

list
stream
list
stream.tail.tail.tail.tail
stream.tail.tail.tail
stream.tail.tail
stream.tail