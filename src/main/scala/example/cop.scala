package example

import cats.free.{Free, FreeApplicative}
import cats.{Applicative, Monad, ~>}

sealed trait Cop[+X] extends Product with Serializable

sealed trait :+:[H[+_], +T <: Cop[X], +X] extends Cop[X] {
  def eliminate[A](l: H[X] => A, r: T => A): A
}

final case class Inl[H[+_], +T <: Cop[X], +X](head: H[X]) extends :+:[H, T, X] {
  override def eliminate[A](l: H[X] => A, r: T => A) = l(head)
}

final case class Inr[H[+_], +T <: Cop[X], +X](tail: T) extends :+:[H, T, X] {
  override def eliminate[A](l: H[X] => A, r: T => A) = r(tail)
}

object Cop {
  private[this] type IdPlus[+A] = A

  def inject[C <: Cop[X], I[_], X](i: I[X])(implicit inj: Inject[C, I, X]): C =
    inj.apply(i)

  def select[C <: Cop[X], T[_], X](c: C)(implicit sel: Select[C, T, X]): Option[T[X]] =
    sel.apply(c)

  trait Inject[C <: Cop[X], I[_], X] extends Serializable {
    def apply(i: I[X]): C
  }

  object Inject {
    def apply[C <: Cop[X], I[_], X](implicit inj: Inject[C, I, X]): Inject[C, I, X] = inj

    final class HeadInject[H[+_], T <: Cop[X], X] extends Inject[:+:[H, T, X], H, X] {
      override def apply(i: H[X]): :+:[H, T, X] = Inl[H, T, X](i)
    }

    private[this] val headInjectInstance = new HeadInject[IdPlus, CNil[Nothing], Nothing]

    implicit def headInject[H[+_], T <: Cop[X], X]: HeadInject[H, T, X] =
      headInjectInstance.asInstanceOf[HeadInject[H, T, X]]

    final class TailInject[H[+ _], T <: Cop[X], I[_], X](implicit inj: Inject[T, I, X]) extends Inject[:+:[H, T, X], I, X] {
      override def apply(i: I[X]): :+:[H, T, X] = Inr[H, T, X](inj(i))
    }

    implicit def tailInject[H[+ _], T <: Cop[X], I[_], X](implicit inj: Inject[T, I, X]): TailInject[H, T, I, X] = new TailInject[H, T, I, X]
  }

  trait Select[C <: Cop[X], T[_], X] extends Serializable {
    def apply(c: C): Option[T[X]]
  }

  object Select {
    def apply[C <: Cop[X], T[_], X](implicit select: Select[C, T, X]): Select[C, T, X] = select

    final class HeadSelect[H[+ _], T <: Cop[X], X] extends Select[:+:[H, T, X], H, X] {
      override def apply(c: :+:[H, T, X]): Option[H[X]] =
        c match {
          case Inl(head) => Some(head)
          case _ => None
        }
    }

    private[this] val headSelectInstance = new HeadSelect[IdPlus, CNil[Nothing], Nothing]

    implicit def headSelect[H[+ _], T <: Cop[X], X]: HeadSelect[H, T, X] =
      headSelectInstance.asInstanceOf[HeadSelect[H, T, X]]

    final class TailSelect[H[+ _], T <: Cop[X], S[_], X](implicit sel: Select[T, S, X]) extends Select[:+:[H, T, X], S, X] {
      override def apply(c: :+:[H, T, X]): Option[S[X]] =
        c match {
          case Inr(tail) => sel.apply(tail)
          case _ => None
        }
    }

    implicit def tailSelect[H[+ _], T <: Cop[X], S[_], X](implicit sel: Select[T, S, X]): TailSelect[H, T, S, X] =
      new TailSelect[H, T, S, X]
  }
}

sealed trait CNil[+X] extends Cop[X] {
  /** Call this when you hit the CNil case in pattern matching to make the match exhaustive and safe. */
  def impossible: Nothing
}

sealed trait Pop[Z[_]] extends Product with Serializable {
  type CopType[+X] <: Cop[X]

  def consume[X](c: CopType[X]): Z[X]

  def :*:[H[+ _]](h: H ~> Z): :*:[H, this.type, Z] = example.:*:(h, this)
}

final case class :*:[H[+_], +T <: Pop[Z], Z[_]](head: H ~> Z, tail: T) extends Pop[Z] {
  override type CopType[+X] = :+:[H, tail.CopType[X], X]

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
  type PopType[Z[_]] <: Pop.Aux[CopType, Z]

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
