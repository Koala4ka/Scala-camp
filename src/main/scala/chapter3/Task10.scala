package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons

object Task10 extends App {
 // EXERCISE 3.10 “foldLeft” (p.44)
 @annotation.tailrec
 def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
   case Nil => z
   case Cons(h, t) => foldLeft(t, f(z, h))(f)
 }

}
