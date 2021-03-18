package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons

object Task2 extends App {
  //EXERCISE 3.2 “tail” (p.40)
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) => t

    }
}
