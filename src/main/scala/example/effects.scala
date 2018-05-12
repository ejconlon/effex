package example

import cats.Monoid
import cats.effect.IO

trait Effects[F[_]] {
  def reader[R](initial: R): F[Reader[F, R]]
  def writer[W](implicit monW: Monoid[W]): F[Writer[F, W]]
  def state[S](initial: S): F[State[F, S]]
}

object Effects {
  implicit object IOEffects extends Effects[IO] {
    override def reader[R](initial: R): IO[Reader[IO, R]] =
      Reader.io[R](initial)

    override def writer[W](implicit monW: Monoid[W]): IO[Writer[IO, W]] =
      Writer.io[W]

    override def state[S](initial: S): IO[State[IO, S]] =
      State.io[S](initial)
  }
}
