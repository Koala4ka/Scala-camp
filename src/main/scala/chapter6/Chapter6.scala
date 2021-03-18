package chapter6

object Chapter6 extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {

    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = Simple(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    //    EXERCISE 6.1 “positiveInt” (p.79)
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, rnd) = rng.nextInt
      if (i > 0) (i, rnd)
      else if (i == Int.MinValue) (-(i + 1), rnd)
      else (-i, rnd)
    }

    //    EXERCISE 6.2 “double” (p.80)
    def double(rng: RNG): (Double, RNG) = {
      val (i, rng2) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), rng2)
    }

    //    EXERCISE 6.3 “tuples” (p.80)
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng2) = rng.nextInt
      val (d, rng3) = double(rng2)
      ((i, d), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), rng2) = intDouble(rng)
      ((d, i), rng2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng2) = double(rng)
      val (d2, rng3) = double(rng2)
      val (d3, rng4) = double(rng3)
      ((d1, d2, d3), rng4)
    }

    //    EXERCISE 6.4 “ints” (p.80)
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      if (count <= 0) (Nil, rng)
      else {
        val (x, rng2) = rng.nextInt
        val (xs, rng3) = ints(count - 1)(rng2)
        (x :: xs, rng3)
      }

    //    EXERCISE 6.6 “RNG.double(map)” (p.82)
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)
        (f(a, b), rng2)
      }

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] =
      both(int, double)

    val randDoubleInt: Rand[(Double, Int)] =
      both(double, int)


    //    EXERCISE 6.8 “sequence/ints” (p.82) H

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng1) = f(rng)
        g(a)(rng1)
      }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
      }

    //    EXERCISE 6.9 “flatMap/positiveInt” (p.83)

    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))
  }

}