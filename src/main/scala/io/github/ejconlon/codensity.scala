package io.github.ejconlon

import cats.effect.IO

object CodIO { cls =>
  def pure[A](value: A): CodIO[A] =
    new CodIO[A] {
      override def apply[Z](act: A => IO[Z]): IO[Z] =
        act(value)
    }

  def raiseError[A](e: Throwable): CodIO[A] =
    new CodIO[A] {
      override def apply[Z](act: A => IO[Z]): IO[Z] =
        IO.raiseError(e)
    }

  def liftIO[A](ioa: IO[A]): CodIO[A] =
    new CodIO[A] {
      override def apply[Z](act: A => IO[Z]): IO[Z] =
        ioa.flatMap(act)
    }

  /**
    * This function is the entire reason for this complicated scheme.
    * We acquire some resource with some configuration `X` and need to
    * make sure we release it *no matter what* after we use it.
    * [[IO.bracket]] lets us be sure that we call the release function.
    * We want to make sure callers can do whatever they want with the resource
    * so we allow them to return whatever type of value they want (the `Z` type
    * parameter on [[CodIO#apply]]).
    */
  def bracket[X, R](acquire: X => IO[R])(release: R => IO[Unit])(info: X): CodIO[R] =
    new CodIO[R] {
      override def apply[Z](act: R => IO[Z]): IO[Z] =
        acquire(info).bracket(act)(release)
      }

  val unit: CodIO[Unit] = pure(())
}

/**
  * A kind of Codensity Monad over IO. Big words that basically mean we hide some value
  * and only let it out through applying a function. See [[CodIO.bracket]] for the motivation.
  *
  * This is covariant in `A` (hence [[CodIO#map]]) even though it doesn't look like it!
  * Just think, "I can get an `A` out of this."
  *
  * NOTE: This is not stack-safe. Don't write big looping programs with this.
  * It's also difficult (if not impossible) to seal this due to the forall on [[CodIO#apply]].
  * There is probably also some work to cleanup sync/async IO but I am not an expert.
  *
  * If you want to go crazy, read this (but you really shouldn't):
  * https://hackage.haskell.org/package/kan-extensions-5.1/docs/Control-Monad-Codensity.html
  */
trait CodIO[A] { self =>
  def apply[Z](act: A => IO[Z]): IO[Z]

  final def lowerIO: IO[A] =
    self.apply { IO.pure }

  final def map[B](f: A => B): CodIO[B] =
    new CodIO[B] {
      override def apply[Z](act: B => IO[Z]): IO[Z] =
        self.apply(f.andThen(act))
    }

  final def mapIO[B](f: A => IO[B]): CodIO[B] =
    new CodIO[B] {
      override def apply[Z](act: B => IO[Z]): IO[Z] =
        self.apply { f(_).flatMap(act) }
    }

  final def flatMap[B](f: A => CodIO[B]): CodIO[B] =
    new CodIO[B] {
      override def apply[Z](act: B => IO[Z]): IO[Z] =
        self.apply { f(_).apply(act) }
    }
}
