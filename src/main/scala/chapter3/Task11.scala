package chapter3

import chapter3.Task1.List
import chapter3.Task10.foldLeft

object Task11 extends App{
 // EXERCISE 3.11 “sum/product/length” (p.44)
 def sum2[A](l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product2[A](l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)
}
