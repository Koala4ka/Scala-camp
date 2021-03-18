package chapter4

object Task7 extends App{
  //EXERCISE 4.7 “eitherFunctions” (p.61)
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case x :: xs => f(x) flatMap(xx => traverse(xs)(f) map (xx :: _))
    }
}
