package chapter3

import chapter3.Task25.{Branch, Leaf, Tree}

object Task27 extends App{
 //EXERCISE 3.27 “depth” (p.48)
 def depth[A](tree: Tree[A]): Int = tree match {
   case Leaf(_) => 1
   case Branch(left, right) => depth(left).max(depth(right)) + 1
 }
}
