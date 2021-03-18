package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons
import chapter3.Task10.foldLeft

object Task12 extends App {
  //EXERCISE 3.12 “reverse” (p.45)
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

}
