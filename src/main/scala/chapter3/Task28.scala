package chapter3

import chapter3.Task25.{Branch, Leaf, Tree}

object Task28 extends App{
//EXERCISE 3.28 “map” (p.48)
def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
  case Leaf(a) => Leaf(f(a))
  case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}
}
