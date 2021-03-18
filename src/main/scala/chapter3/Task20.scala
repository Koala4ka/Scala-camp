package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task6.foldRight
import chapter3.Task14.append

object Task20 extends App{
 // EXERCISE 3.20 “flatMap” (p.46)
 def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
   foldRight(as, Nil: List[B])((a, l) => append(f(a), l))
 }
}
