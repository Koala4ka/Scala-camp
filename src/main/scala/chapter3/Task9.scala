package chapter3

import chapter3.Task1.List
import chapter3.Task6.foldRight

object Task9 extends App{
  //EXERCISE 3.9 “length” (p.44)
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)
}
