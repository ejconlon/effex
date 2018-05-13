/**
  * Product and Coproduct defs adapted from shapeless
  * https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/ops/coproduct.scala
  * https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/hlists.scala
  */
package io.github.ejconlon.effex

import cats.free.{Free, FreeApplicative}
import cats.{Applicative, Monad, ~>}

sealed trait Cop[+X] extends Product with Serializable

sealed trait :+:[H[+_], +X, +TX <: Cop[X]] extends Cop[X] {
  def eliminate[A](l: H[X] => A, r: TX => A): A
}

final case class Inl[H[+_], +X, +TX <: Cop[X]](head: H[X]) extends :+:[H, X, TX] {
  override def eliminate[A](l: H[X] => A, r: TX => A) = l(head)
}

final case class Inr[H[+_], +X, +TX <: Cop[X]](tail: TX) extends :+:[H, X, TX] {
  override def eliminate[A](l: H[X] => A, r: TX => A) = r(tail)
}

sealed trait CNil[+X] extends Cop[X] {
  def impossible: Nothing
}

object Cop {
  private[this] type IdPlus[+A] = A

  def inject[CX <: Cop[X], I[_], X](i: I[X])(implicit inj: Inject[CX, I, X]): CX =
    inj.apply(i)

  def select[CX <: Cop[X], T[_], X](c: CX)(implicit sel: Select[CX, T, X]): Option[T[X]] =
    sel.apply(c)

  trait Inject[C <: Cop[X], I[_], X] extends Serializable {
    def apply(i: I[X]): C
  }

  object Inject {
    def apply[CX <: Cop[X], I[_], X](implicit inj: Inject[CX, I, X]): Inject[CX, I, X] = inj

    final class HeadInject[H[+_], TX <: Cop[X], X] extends Inject[:+:[H, X, TX], H, X] {
      override def apply(i: H[X]): :+:[H, X, TX] = Inl[H, X, TX](i)
    }

    private[this] val headInjectInstance = new HeadInject[IdPlus, CNil[Nothing], Nothing]

    implicit def headInject[H[+_], TX <: Cop[X], X]: HeadInject[H, TX, X] =
      headInjectInstance.asInstanceOf[HeadInject[H, TX, X]]

    final class TailInject[H[+ _], TX <: Cop[X], I[_], X](implicit inj: Inject[TX, I, X]) extends Inject[:+:[H, X, TX], I, X] {
        override def apply(i: I[X]): :+:[H, X, TX] = Inr[H, X, TX](inj(i))
    }

    implicit def tailInject[H[+ _], TX <: Cop[X], I[_], X](implicit inj: Inject[TX, I, X]): TailInject[H, TX, I, X] =
      new TailInject[H, TX, I, X]
  }

  trait Select[CX <: Cop[X], T[_], X] extends Serializable {
    def apply(c: CX): Option[T[X]]
  }

  object Select {
    def apply[CX <: Cop[X], T[_], X](implicit select: Select[CX, T, X]): Select[CX, T, X] = select

    final class HeadSelect[H[+ _], TX <: Cop[X], X] extends Select[:+:[H, X, TX], H, X] {
      override def apply(c: :+:[H, X, TX]): Option[H[X]] =
        c match {
          case Inl(head) => Some(head)
          case _ => None
        }
    }

    private[this] val headSelectInstance = new HeadSelect[IdPlus, CNil[Nothing], Nothing]

    implicit def headSelect[H[+ _], TX <: Cop[X], X]: HeadSelect[H, TX, X] =
      headSelectInstance.asInstanceOf[HeadSelect[H, TX, X]]

    final class TailSelect[H[+ _], TX <: Cop[X], S[_], X](implicit sel: Select[TX, S, X]) extends Select[:+:[H, X, TX], S, X] {
      override def apply(c: :+:[H, X, TX]): Option[S[X]] =
        c match {
          case Inr(tail) => sel.apply(tail)
          case _ => None
        }
    }

    implicit def tailSelect[H[+ _], TX <: Cop[X], S[_], X](implicit sel: Select[TX, S, X]): TailSelect[H, TX, S, X] =
      new TailSelect[H, TX, S, X]
  }
}

sealed trait Pop[Z[_]] extends Product with Serializable {
  type CopType[+X] <: Cop[X]

  def consume[X](c: CopType[X]): Z[X]

  final def :*:[H[+ _]](h: H ~> Z): :*:[H, Z, this.type] = _root_.io.github.ejconlon.effex.:*:(h, this)
}

final case class :*:[H[+_], Z[_], +T <: Pop[Z]](head: H ~> Z, tail: T) extends Pop[Z] {
  override type CopType[+X] = :+:[H, X, tail.CopType[X]]

  override def consume[X](c: CopType[X]): Z[X] =
    c match {
      case Inl(ch) => head.apply[X](ch)
      case Inr(ct) => tail.consume[X](ct.asInstanceOf[tail.CopType[X]])
    }
}

final case class PNil[Z[_]]() extends Pop[Z] {
  override type CopType[+X] = CNil[X]

  override def consume[X](c: CopType[X]): Z[X] = c.impossible
}

object Pop {
  type Aux[C0[+_], Z[_]] = Pop[Z] { type CopType[+X] = C0[X] }
}

trait Language {
  type CopType[+X] <: Cop[X]
  type PopType[Z[_]] = Pop.Aux[CopType, Z]

  final def inject[F[_], X](f: F[X])(implicit inj: Cop.Inject[CopType[X], F, X]): CopType[X] =
    inj.apply(f)

  final def select[F[_], X](c: CopType[X])(implicit sel: Cop.Select[CopType[X], F, X]): Option[F[X]] =
    sel.apply(c)

  final def consume[Z[_], X](c: CopType[X], p: PopType[Z]): Z[X] =
    p.consume[X](c)

  final class Trans[Z[_]](p: PopType[Z]) extends ~>[CopType, Z] {
    override def apply[A](c: CopType[A]): Z[A] =
      p.consume[A](c)
  }

  final def injectFree[F[_], X](f: F[X])(implicit inj: Cop.Inject[CopType[X], F, X]): Free[CopType, X] =
    Free.liftF(inject[F, X](f))

  final def consumeFree[Z[_], X](f: Free[CopType, X], p: PopType[Z])(implicit monad: Monad[Z]): Z[X] =
    f.foldMap(new Trans[Z](p))

  final def transFree[Z[_], X](f: Free[CopType, X], p: PopType[Z]): Free[Z, X] =
    f.mapK(new Trans[Z](p))

  final def injectFreeApp[F[_], X](f: F[X])(implicit inj: Cop.Inject[CopType[X], F, X]): FreeApplicative[CopType, X] =
    FreeApplicative.lift(inject[F, X](f))

  final def consumeFreeApp[Z[_], X](f: FreeApplicative[CopType, X], p: PopType[Z])(implicit app: Applicative[Z]): Z[X] =
    f.foldMap(new Trans[Z](p))

  final def transFreeApp[Z[_], X](f: FreeApplicative[CopType, X], p: PopType[Z]): FreeApplicative[Z, X] =
    f.compile(new Trans[Z](p))
}
