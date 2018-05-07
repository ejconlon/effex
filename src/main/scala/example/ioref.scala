/**
  * Copied from https://github.com/scalaz/scalaz/blob/series/8.0.x/effect/shared/src/main/scala/scalaz/effect/IORef.scala
  * and converted from scalaz 8 to cats
  */
package example

import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO

/**
  * A mutable atomic reference for the `IO` monad. This is the `IO` equivalent of
  * a volatile `var`, augmented with atomic operations, which make it useful as a
  * reasonably efficient (if low-level) concurrency primitive.
  *
  * {{{
  * for {
  *   ref <- IORef(2)
  *   v   <- ref.modify(_ + 3)
  *   _   <- putStrLn("Value = " + v.debug) // Value = 5
  * } yield ()
  * }}}
  */
final class IORef[A] private (private val value: AtomicReference[A]) extends AnyVal {

  /**
    * Reads the value from the `IORef`.
    */
  def read[E]: IO[A] = IO { value.get }

  /**
    * Writes a new value to the `IORef`, with a guarantee of immediate
    * consistency (at some cost to performance).
    */
  def write[E](a: A): IO[Unit] = IO { value.set(a) }

  /**
    * Writes a new value to the `IORef` without providing a guarantee of
    * immediate consistency.
    */
  def writeLater[E](a: A): IO[Unit] = IO { value.lazySet(a) }

  /**
    * Attempts to write a new value to the `IORef`, but aborts immediately under
    * concurrent modification of the value by other fibers.
    */
  def tryWrite[E](a: A): IO[Boolean] = IO { value.compareAndSet(value.get, a) }

  /**
    * Atomically modifies the `IORef` with the specified function. This is not
    * implemented in terms of `modifyFold` purely for performance reasons.
    */
  def modify[E](f: A => A): IO[A] = IO {
    var loop    = true
    var next: A = null.asInstanceOf[A]

    while (loop) {
      val current = value.get

      next = f(current)

      loop = !value.compareAndSet(current, next)
    }

    next
  }

  /**
    * Atomically modifies the `IORef` with the specified function, which computes
    * a return value for the modification. This is a more powerful version of
    * `modify`.
    */
  def modifyFold[E, B](f: A => (B, A)): IO[B] = IO {
    var loop = true
    var b: B = null.asInstanceOf[B]

    while (loop) {
      val current = value.get

      val tuple = f(current)

      b = tuple._1

      loop = !value.compareAndSet(current, tuple._2)
    }

    b
  }

  /**
    * Attempts to atomically modify the `IORef` with the specified function, but
    * aborts immediately under concurrent modification of the value by other
    * fibers.
    */
  def tryModify[E](f: A => A): IO[Option[A]] = IO {
    val current = value.get

    val next = f(current)

    if (value.compareAndSet(current, next)) Some(next)
    else None
  }

  /**
    * Attempts to atomically modify the `IORef` with the specified function, but
    * aborts immediately under concurrent modification of the value by other
    * fibers.
    */
  def tryModifyFold[E, B](f: A => (B, A)): IO[Option[B]] = IO {
    val current = value.get

    val tuple = f(current)

    if (value.compareAndSet(current, tuple._2)) Some(tuple._1)
    else None
  }
}

object IORef {

  /**
    * Creates a new `IORef` with the specified value.
    */
  def apply[E, A](a: A): IO[IORef[A]] = IO { new IORef[A](new AtomicReference(a)) }
}
