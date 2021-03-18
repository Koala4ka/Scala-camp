package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons

object Task22 extends App{
 // EXERCISE 3.22 “sumEach” (p.46)
 def addLists(as: List[Int], bs: List[Int]): List[Int] = {
   def loop(as: List[Int], bs: List[Int], sums: List[Int]): List[Int] = (as, bs) match {
     case (Cons(a, ta), Cons(b, tb)) => Cons(a + b, loop(ta, tb, sums))
     case _ => sums
   }

   loop(as, bs, Nil)
 }
}
