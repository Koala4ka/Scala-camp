package chapter2

object Task4 extends App{
 // “curry” (p.30) H
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

}
