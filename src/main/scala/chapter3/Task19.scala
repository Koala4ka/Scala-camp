package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons
import chapter3.Task6.foldRight

object Task19 extends App {
 //EXERCISE 3.19 “filter” (p.46)
 def filter[A](as: List[A])(f: A => Boolean): List[A] = {
   foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)
 }
}
