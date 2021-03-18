package chapter3

import chapter3.Task1.List
import chapter3.Task1.Nil
import chapter3.Task1.Cons

object Task6 extends App{
 // EXERCISE 3.6 “init” (p.42)
 def init[A](l: List[A]): List[A] = l match {
   case Nil => sys.error("init of empty list")
   case Cons(_, Nil) => Nil
   case Cons(h, t) => Cons(h, init(t))
 }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

}
