package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons
import chapter3.Task6.foldRight

object Task16 extends App{
  //EXERCISE 3.16 “inc” (p.45)
  def mapInts(xs: List[Int]): List[Int] = {
    foldRight(xs, Nil: List[Int])((x, acc) => Cons(x + 1, acc))
  }
}
