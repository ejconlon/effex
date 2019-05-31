package io.github.ejconlon.effex

import cats.{Applicative, Id, ~>}

trait HKD[D[_[_]]] {
  import HKD.ApplicativeTrans

  def compile[F[_], G[_]](data: D[F], trans: F ~> G): D[G]

  def fold[F[_]](data: D[F])(implicit app: Applicative[F]): F[D[Id]]

  def foldMap[F[_], G[_]](data: D[F], trans: F ~> G)(implicit app: Applicative[G]): G[D[Id]] =
    fold[G](compile[F, G](data, trans))

  def pure[F[_]](data: D[Id])(implicit app: Applicative[F]): D[F] =
    compile[Id, F](data, new ApplicativeTrans[F])
}

object HKD {
  final class ApplicativeTrans[F[_]](implicit app: Applicative[F]) extends ~>[Id, F] {
    override def apply[A](value: A): F[A] = app.pure(value)
  }

  def apply[D[_[_]]](implicit hkd: HKD[D]): HKD[D] = hkd

  implicit def fromHopLike[D[_[_]]](implicit hopLike: HopLike[D]): HKD[D] = new HopLikeHKD[D]
}

sealed trait Hop[F[_]] extends Product with Serializable {
  type Related[G[_]] <: Hop[G]

  def compile[G[_]](trans: F ~> G): Related[G]

  def fold(implicit app: Applicative[F]): F[Related[Id]]

  def foldMap[G[_]](trans: F ~> G)(implicit app: Applicative[G]): G[Related[Id]]

  final def :@:[A](h: F[A]): :@:[F, A, this.type] = _root_.io.github.ejconlon.effex.:@:(h, this)
}

final case class :@:[F[_], A, T <: Hop[F]](head: F[A], tail: T) extends Hop[F] {
  override type Related[G[_]] = :@:[G, A, tail.Related[G]]

  override def compile[G[_]](trans: F ~> G): Related[G] =
    _root_.io.github.ejconlon.effex.:@:[G, A, tail.Related[G]](trans.apply(head), tail.compile(trans))

  override def fold(implicit app: Applicative[F]): F[Related[Id]] =
    app.map2(head, tail.fold) { (h, t) =>
      _root_.io.github.ejconlon.effex.:@:[Id, A, tail.Related[Id]](h, t)
    }

  override def foldMap[G[_]](trans: F ~> G)(implicit app: Applicative[G]): G[Related[Id]] =
    app.map2(trans.apply(head), tail.foldMap(trans)) { (h, t) =>
      _root_.io.github.ejconlon.effex.:@:[Id, A, tail.Related[Id]](h, t)
    }
}

final case class HNil[F[_]]() extends Hop[F] {
  override type Related[G[_]] = HNil[G]

  override def compile[G[_]](trans: F ~> G): Related[G] = HNil[G]()

  override def fold(implicit app: Applicative[F]): F[Related[Id]] = app.pure(HNil[Id]())

  override def foldMap[G[_]](trans: F ~> G)(implicit app: Applicative[G]): G[Related[Id]] = app.pure(HNil[Id]())
}

trait HopLike[D[_[_]]] {
  type H[F[_]] <: Hop[F]

  def toHop[F[_]](d: D[F]): H[F]

  def fromRelated[F[_], G[_]](r: H[F]#Related[G]): D[G]
}

object HopLike {
  def apply[D[_[_]]](implicit hopLike: HopLike[D]): HopLike[D] = hopLike
}

class HopLikeHKD[D[_[_]]](implicit hopLike: HopLike[D]) extends HKD[D] {
  override def compile[F[_], G[_]](data: D[F], trans: F ~> G): D[G] =
    hopLike.fromRelated[F, G](hopLike.toHop[F](data).compile[G](trans))

  override def fold[F[_]](data: D[F])(implicit app: Applicative[F]): F[D[Id]] = {
    // the redundant cast is necessary to work around a surprise compiler bug resulting
    // from typedef application of Id forcing subtype variance annotations on F
    val fhid = hopLike.toHop[F](data).fold.asInstanceOf[F[hopLike.H[F]#Related[Id]]]
    app.map[hopLike.H[F]#Related[Id], D[Id]](fhid)(hopLike.fromRelated[F, Id])
  }

  override def foldMap[F[_], G[_]](data: D[F], trans: F ~> G)(implicit app: Applicative[G]): G[D[Id]] = {
    // see above
    val ghid = hopLike.toHop[F](data).foldMap(trans).asInstanceOf[G[hopLike.H[F]#Related[Id]]]
    app.map[hopLike.H[F]#Related[Id], D[Id]](ghid)(hopLike.fromRelated[F, Id])
  }
}
