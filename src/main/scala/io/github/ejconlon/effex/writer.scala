package io.github.ejconlon.effex

import cats.Monoid
import cats.effect.IO

trait Writer[F[_], W] {
  def writer[A](value: A, message: W): F[A]

  def tell(message: W): F[Unit]

  def listen[A](act: F[A]): F[(A, W)]

  def listens[A, B](act: F[A])(f: W => B): F[(A, B)]

  def concentrate: F[W]

  def pass[A](act: F[(A, W => W)]): F[A]

  def censor[A](act: F[A])(f: W => W): F[A]
}

object Writer {
  def io[W](implicit monW: Monoid[W]): IO[Writer[IO, W]] =
    for {
      ref <- IORef(monW.empty)
    } yield new Writer[IO, W] {
      override def writer[A](value: A, message: W): IO[A] =
        ref.modify(monW.combine(_, message)).map { _ => value }

      override def tell(message: W): IO[Unit] =
        writer[Unit]((), message)

      override def listen[A](act: IO[A]): IO[(A, W)] =
        act.flatMap { a => ref.read.map { (a, _) } }

      override def listens[A, B](act: IO[A])(f: W => B): IO[(A, B)] =
        act.flatMap { a => ref.read.map { w => (a, f(w)) } }

      override def concentrate: IO[W] = ref.read

      override def pass[A](act: IO[(A, W => W)]): IO[A] =
        act.flatMap { case (a, f) => ref.modify(f).map { _ => a } }

      override def censor[A](act: IO[A])(f: W => W): IO[A] =
        act.flatMap { a => ref.modify(f).map { _ => a } }
    }
}
