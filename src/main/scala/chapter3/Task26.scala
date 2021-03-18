package chapter3

import chapter3.Task25.{Branch, Leaf, Tree}

object Task26 extends App{
  //EXERCISE 3.26 “maximum” (p.48)
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left).max(maximum(right))
  }
}
