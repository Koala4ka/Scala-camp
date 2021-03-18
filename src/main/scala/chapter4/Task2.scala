package chapter4

object Task2 extends App{
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = ???
    def flatMap[B](f: A => Option[B]): Option[B] = ???
    def getOrElse[B >: A](default: => B): B = ???
    def orElse[B >: A](ob: => Option[B]): Option[B] = ???
    def filter(f: A => Boolean): Option[A] = ???
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //EXERCISE 4.2 “variance” (p.56)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
