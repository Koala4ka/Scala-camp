package chapter2

object Task3 extends App{
 //   2.3 “partial1” (p.29) H
    def curry[A, B, C](f: (A, B) => C): A => B => C =
      (a: A) => (b: B) => f(a, b)

    def f(a: Int, b: Int): Int = a + b
    def g(a: Int)(b: Int): Int = a + b

    curry(f)(1)(1) == f(1, 1)
    curry(f)(1)(1) == g(1)(1)

}

