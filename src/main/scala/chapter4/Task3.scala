package chapter4

object Task3  extends App{
 // EXERCISE 4.3 “map2” (p.59)
 def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
   a.flatMap(va => b.map(vb => f(va, vb)))
}
