package chapter2

object Task5 extends App{
  //“uncurry” (p.30) O
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}

