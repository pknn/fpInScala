package FunctionalProgramming

object FunctionalProgramming {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    s"The absolute value of $x is ${abs(x)}"
  }

  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, accumulator: Int = 1): Int =
      if (n <= 0) accumulator
      else go(n - 1, n * accumulator)

    go(n)
  }

  def fibonacci(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, previous: Int = 0, current: Int = 1): Int =
      if (n == 0) previous
      else go(n - 1, current, previous + current)

    go(n)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    s"The $name for $n is ${f(n)}"
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @scala.annotation.tailrec
    def go(n: Int = 0): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else go(n + 1)
    }
    go()
  }

  // Generic Version of findFirst()
  def findFirst[T](as: Array[T], p: T => Boolean): Int = {
    @scala.annotation.tailrec
    def go(n: Int = 0): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else go(n + 1)
    }
    go()
  }

  def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def go(n: Int = 0): Boolean = {
      if (n >= as.length - 1) true
      else if (f(as(n), as(n + 1))) false
      else go(n + 1)
    }
    go()
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}