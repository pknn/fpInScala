package FunctionalDataStructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  } // yield 3

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def foldRight[A, B](as: List[A], initial: B)(f: (A, B) => B): B = as match {
    case Nil => initial
    case Cons(h, t) => f(h, foldRight(t, initial)(f))
  }

  def sumFR(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def productFR(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  @scala.annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil | Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def lengthFR[A](as: List[A]): Int =
    foldRight(as, 0)((_, accumulator) => accumulator + 1)

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], initial: B)(f: (B, A) => B): B = as match {
    case Nil => initial
    case Cons(h, t) => foldLeft(t, f(initial, h))(f)
  }

  def sumFL(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productFL(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthFL[A](as: List[A]): Int =
    foldLeft(as, 0)((accumulator, _) => accumulator + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((accumulator, h) => Cons(h, accumulator))

  def foldRightFL[A, B](as: List[A], initial: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), initial)((b, a) => f(a, b))

  def foldLeftFR[A, B](as: List[A], initial: B)(f: (B, A) => B): B =
    foldRightFL(as, (b: B) => b)((a, g) => b => g(f(b, a)))(initial)

  def appendFR[A](a1: List[A], a2: List[A]): List[A] =
    foldRightFL(a1, a2)(Cons(_, _))

  def concat[A](aas: List[List[A]]): List[A] =
    foldRightFL(aas, Nil: List[A])(append)

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightFL(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A]=
    foldRightFL(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(ns1: List[Int], ns2: List[Int]): List[Int] = (ns1, ns2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    case _ => Nil
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @scala.annotation.tailrec
  def startsWith[A](as: List[A], prefix: List[A]): Boolean = (as, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }
}
