package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons
import chapter3.Task6.foldRight

object Task18 extends App{
  //EXERCISE 3.18 “map” (p.46)
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))
  }
}
