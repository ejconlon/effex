package io.github.ejconlon.effex

import cats.{Applicative, Id, ~>}
import org.scalatest.FunSuite

object HKDSpec {

  case class Child[F[_]](nickname: F[String], age: F[Int])

  case class Adult[F[_]](job: F[String], innerChild: Child[F])

  trait HasHKDs {
    implicit def childHKD: HKD[Child]

    implicit def adultHKD: HKD[Adult]
  }

  object Manual extends HasHKDs {

    object ChildHKDManual extends HKD[Child] {
      override def compile[F[_], G[_]](data: Child[F], trans: F ~> G): Child[G] =
        Child[G](
          nickname = trans(data.nickname),
          age = trans(data.age)
        )

      override def fold[F[_]](data: Child[F])(implicit app: Applicative[F]): F[Child[Id]] =
        app.map2(data.nickname, data.age) { (nickname, age) =>
          Child[Id](
            nickname = nickname,
            age = age
          )
        }
    }

    class AdultHKDManual(implicit childHKD: HKD[Child]) extends HKD[Adult] {
      override def compile[F[_], G[_]](data: Adult[F], trans: ~>[F, G]): Adult[G] =
        Adult[G](
          job = trans(data.job),
          innerChild = childHKD.compile(data.innerChild, trans)
        )

      override def fold[F[_]](data: Adult[F])(implicit app: Applicative[F]): F[Adult[Id]] =
        app.map2(data.job, childHKD.fold(data.innerChild)) { (job, innerChild) =>
          Adult[Id](
            job = job,
            innerChild = innerChild
          )
        }
    }

    override implicit def childHKD: HKD[Child] = ChildHKDManual

    override implicit def adultHKD: HKD[Adult] = new AdultHKDManual
  }

  object Derived extends HasHKDs {

    object ChildHopLike extends HopLike[Child] {
      override type H[F[_]] = :@:[F, String, :@:[F, Int, HNil[F]]]

      override def toHop[F[_]](d: Child[F]): H[F] =
        :@:(d.nickname, :@:(d.age, HNil[F]()))

      override def fromRelated[F[_], G[_]](r: H[F]#Related[G]): Child[G] =
        Child[G](
          nickname = r.head,
          age = r.tail.head
        )
    }

    class AdultHopLike(implicit val childHopLike: HopLike[Child]) extends HopLike[Adult] {
      override type H[F[_]] = :@:[F, String, childHopLike.H[F]]

      override def toHop[F[_]](d: Adult[F]): H[F] =
        :@:(d.job, childHopLike.toHop[F](d.innerChild))

      override def fromRelated[F[_], G[_]](r: H[F]#Related[G]): Adult[G] =
        Adult[G](
          job = r.head,
          innerChild = childHopLike.fromRelated[F, G](r.tail)
        )
    }

    implicit def childHopLike: HopLike[Child] = ChildHopLike

    implicit def adultHopLike: HopLike[Adult] = new AdultHopLike

    override implicit def childHKD: HKD[Child] = HKD.fromHopLike[Child]

    override implicit def adultHKD: HKD[Adult] = HKD.fromHopLike[Adult]
  }
}

class HKDSpec extends FunSuite {
  test("empty") {
    assert((1 + 1) == 2)
  }
}
