package io.github.ejconlon

import cats.{Applicative, Id, ~>}

trait HKD[D[_[_]]] {
  import HKD.ApplicativeTrans

  def compile[F[_], G[_]](data: D[F], trans: F ~> G): D[G]

  def fold[F[_]](data: D[F])(implicit applicativeF: Applicative[F]): F[D[Id]]

  def foldMap[F[_], G[_]](data: D[F], trans: F ~> G)(implicit app: Applicative[G]): G[D[Id]] =
    fold[G](compile[F, G](data, trans))

  def pure[F[_]](data: D[Id])(implicit app: Applicative[F]): D[F] =
    compile[Id, F](data, new ApplicativeTrans[F])
}

object HKD {
  final class ApplicativeTrans[F[_]](implicit applicativeF: Applicative[F]) extends ~>[Id, F] {
    override def apply[A](value: A): F[A] = applicativeF.pure(value)
  }
}
