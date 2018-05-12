package io.github.ejconlon

import cats.effect.IO

trait State[F[_], S] {
  def get: F[S]

  def put(s: S): F[Unit]

  def state[A](f: S => (A, S)): F[A]

  def modify(f: S => S): F[Unit]

  def gets[A](f: S => A): F[A]
}

object State {
  def io[S](initial: S): IO[State[IO, S]] =
    for {
      ref <- IORef(initial)
    } yield new State[IO, S] {
      override def get: IO[S] =
        ref.read

      override def put(s: S): IO[Unit] =
        ref.write(s)

      override def state[A](f: S => (A, S)): IO[A] =
        ref.modifyFold(f)

      override def modify(f: S => S): IO[Unit] =
        ref.modify(f).map { _ => () }

      override def gets[A](f: S => A): IO[A] =
        ref.read.map(f)
    }
}
