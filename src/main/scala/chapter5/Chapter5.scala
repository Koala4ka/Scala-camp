package chapter5

object Chapter5 extends App {

  import Stream._

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    //EXERCISE 5.1 “toList” (p.69)
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }

    //EXERCISE 5.2 “take” (p.69)
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    //EXERCISE 5.3 “takeWhile” (p.70)
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }

    //EXERCISE 5.4 “forAll” (p.70)
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((h, r) => p(h) && r)

    // this match {
    //   case Cons(h, _) if !p(h()) => false
    //   case Cons(_, t) => t() forAll p
    //   case _ => true
    // }

    //EXERCISE 5.5 “takeWhile(foldRight)” (p.71)
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

    //EXERCISE 5.6 “map/filter/append” (p.71)
    def headOptionViaFoldRight: Option[A] =
      foldRight[Option[A]](None)((h, _) => Some(h))

    // EXERCISE 5.7 “constant” (p.73)
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
        if (p(h)) cons(h, t)
        else t
      )

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h) append t)

    //EXERCISE 5.8 “from” (p.73)
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //    EXERCISE 5.9 “fibs” (p.73)
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //    EXERCISE 5.10 “unfold” (p.73)
    def fibs: Stream[Int] = {
      def loop(previous: Int, current: Int): Stream[Int] =
        cons(previous, loop(current, previous + current))

      loop(0, 1)
    }

    //    EXERCISE 5.11 “fibs/from/constant/ones” (p.74)
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

    //    EXERCISE 5.12 “map/take/takeWhile/zip/zipAll” (p.74)

    def tails: Stream[Stream[A]] =
      unfold(this) {
        case s@Cons(_, t) => Some((s, t()))
        case Empty => None
      } append Stream(empty)


    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    // tails.map(_.foldRight(z)(f))
      foldRight((z, Stream(z)))((a, b) => {
        lazy val lb = b
        val x = f(a, lb._1)
        (x, cons(x, lb._2))
      })._2
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

  }

}
