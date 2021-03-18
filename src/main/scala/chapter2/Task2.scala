package chapter2

object Task2 extends App {
  // “isSorted” (p.27)
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def iteration(i: Int): Boolean = {
  if (i >= as.length - 1) true
  else !gt(as(i), as(i + 1)) && iteration(i + 1)
}
  iteration(0)
}
}






