package chapter2

object Task1 extends App{
  //“Fibonacci” (p.22 tail calls)
  def fib(num: Int) : Int = {
    @annotation.tailrec
    def getTailRec(index: Int, prev: Int, current: Int): Int = {
      if (index <= 0) current
      else getTailRec(index - 1, prev = prev + current, current = prev)
    }
    getTailRec(num, prev = 1, current = 0)
  }
  println(fib(3))
}



