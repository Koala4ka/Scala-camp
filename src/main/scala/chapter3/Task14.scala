package chapter3

import chapter3.Task1.List
import chapter3.Task1.Cons
import chapter3.Task6.foldRight

object Task14 extends App{
  //EXERCISE 3.14 “append” (p.45)
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))
}
