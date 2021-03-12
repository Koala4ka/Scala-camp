package chapter2

object Task2 extends App {
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {

    def loop(as: Array[A]): Boolean = {
      if (as.length == 1 || as.length == 0) true
      else if (gt(as.head, as.tail.head)) false
      else loop(as.tail)
    }
    loop(as)
  }

}





