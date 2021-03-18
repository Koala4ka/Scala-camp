package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons

object Task23 extends App{
  //EXERCISE 3.23 “zipMap” (p.46)
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    def loop(as: List[A], bs: List[B], sums: List[C]): List[C] = (as, bs) match {
      case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), loop(ta, tb, sums))
      case _ => sums
    }

    loop(as, bs, Nil)
  }

}
