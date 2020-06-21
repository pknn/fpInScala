package Parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

trait Future[+A] {
  private[Parallelism] def apply(k: A => Unit): Unit
}

object NonBlockingPar {
  type Par[+A] = ExecutorService => Future[A]
  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) {
        a => {
          ref.set(a)
          latch.countDown()
        }
      }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => (cb: A => Unit) => cb(a)

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call: Unit = r
      })

    def fork[A](a: => Par[A]): Par[A] =
      es => (cb: A => Unit) => eval(es)(a(es)(cb))

    def delay[A](a: => A): Par[A] =
      es => (cb: A => Unit) => cb(a)

  }
}
