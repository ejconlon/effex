package example

import cats.Applicative
import cats.effect.IO

trait Reader[F[_], R] {
  def ask: F[R]

  def local(f: R => R): Reader[F, R]

  def reader[A](f: R => A): F[A]
}

object Reader {
  def applicative[F[_], R](initial: R)(implicit appF: Applicative[F]): Reader[F, R] =
    new Reader[F, R] {
      override def ask: F[R] =
        appF.pure(initial)

      override def local(f: R => R): Reader[F, R] =
        applicative[F, R](f(initial))

      override def reader[A](f: R => A): F[A] =
        appF.pure(f(initial))
    }

  def io[R](initial: R): IO[Reader[IO, R]] =
    IO.pure(applicative[IO, R](initial))
}
