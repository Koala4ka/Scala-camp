package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons
import chapter3.Task6.foldRight

object Task17 extends App{
 // EXERCISE 3.17 “stringify” (p.46)
 def mapDoubles(xs: List[Double]): List[String] = {
   foldRight(xs, Nil: List[String])((x, acc) => Cons(x.toString, acc))
 }
}
