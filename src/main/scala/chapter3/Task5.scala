package chapter3

import chapter3.Task1.List
import chapter3.Task1.Cons

object Task5 extends App{
 // EXERCISE 3.5 “setHead” (p.41)
 def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
   case Cons(h, t) if f(h) => dropWhile(t, f)
   case _ => l
 }
}
