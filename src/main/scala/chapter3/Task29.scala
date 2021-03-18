package chapter3

import chapter3.Task25.{Branch, Leaf, Tree}

object Task29 extends App{
//EXERCISE 3.29 “fold” (p.49)
def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
  case Leaf(a) => f(a)
  case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
}

  def size2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _ + 1)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree)(a => a)(_.max(_))

  def depth2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)(_.max(_) + 1)

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    def fa(a: A): Tree[B] = Leaf(f(a))

    def fb(b1: Tree[B], b2: Tree[B]): Tree[B] = Branch(b1, b2)

    fold(tree)(fa)(fb)
  }
}

