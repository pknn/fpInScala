package Laziness

trait Stream[+A] {
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A] = List()): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (f(h())) => Stream.cons(h(), t().takeWhile(f))
    case _ => Stream.empty
  }

  def foldRight[B](initial: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(initial)(f))
    case _ => initial
  }

  def exists(f: A => Boolean): Boolean =
    foldRight(false)((h, t) => f(h) || t)

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((h, t) => f(h) && t)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
      if (f(h)) Stream.cons(h, t)
      else t
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))

  def zipWith[B, C](o: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, o)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](o: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(o)((_, _))

  def zipWithAll[B, C](o: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, o)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[S](o: Stream[S]): Boolean =
    zipAll(o).takeWhile(v => v._2.isDefined).forAll(t => t._1 == t._2)

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream.empty)

  def hasSubsequence[S](s: Stream[S]): Boolean =
    tails.exists(_ startsWith s)

  def scanRight[B](initial: B)(f: (A, => B) => B): Stream[B] =
    foldRight((initial, Stream(initial)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(fc: Int = 0, fn: Int = 1): Stream[Int] =
      cons(fc, go(fn, fc + fn))
    go()
  }

  def unfold[A, S](initial: S)(f: S => Option[(A, S)]): Stream[A] = f(initial) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }
}