package PurelyFunctionalState

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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, ns) = rng.nextInt
    if (v < 0)
      (-(v + 1), ns)
    else
      (v, ns)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, ns) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), ns)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (v, ns) => (v % 2 == 0, ns)
    }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, ns) = rng.nextInt
    val (d, nns) = double(ns)
    ((i, d), nns)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, ns) = double(rng)
    val (i, nns) = ns.nextInt
    ((d, i), nns)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, ns1) = double(rng)
    val (d2, ns2) = double(ns1)
    val (d3, ns3) = double(ns2)
    ((d1, d2, d3), ns3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val (ih, ns) = rng.nextInt
      val (it, ns2) = ints(count - 1)(ns)
      (ih :: it, ns2)
    }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (v, ns) = s(rng)
      (f(v), ns)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (va, ns1) = ra(rng)
      val (vb, ns2) = rb(ns1)
      (f(va, vb), ns2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit(List[A]()))((f, accumulator) => map2(f, accumulator)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (v, ns) = f(rng)
      g(v)(ns)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
}
