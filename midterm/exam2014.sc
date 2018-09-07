def iterate[T](x: T)(f: T => T): Stream[T]= {
  x #:: iterate(f(x))(f)
}



def iterated[T](f: T => T):Stream[T => T] = {
  ((x: T) => x) #:: (iterated(f) map (_ compose f))
}
def iterated2[T](f: T => T):Stream[T => T] = {
  iterate((x: T) => x)(_ andThen f)
}

def f(x: Int) = 2*x
iterated(f)(10)(1)
iterated(f)


