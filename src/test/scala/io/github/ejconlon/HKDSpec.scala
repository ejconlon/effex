package io.github.ejconlon

import cats.{Applicative, Id, ~>}
import org.scalatest.FunSuite

object HKDSpec {
  case class Child[F[_]](nickname: F[String], age: F[Int])

  case class Adult[F[_]](job: F[String], innerChild: Child[F])

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

  object ChildHopLikeHKD extends HopLikeHKD[Child] {
    override type H[F[_]] = :@:[F, String, :@:[F, Int, HNil[F]]]

    override def toHop[F[_]](d: Child[F]): H[F] =
      :@:(d.nickname, :@:(d.age, HNil[F]()))

    override def fromRelated[F[_], G[_]](r: H[F]#Related[G]): Child[G] =
      Child[G](
        nickname = r.head,
        age = r.tail.head
      )
  }

  object AdultHopLikeHKD extends HopLikeHKD[Adult] {
    override type H[F[_]] = :@:[F, String, ChildHopLikeHKD.H[F]]

    override def toHop[F[_]](d: Adult[F]): H[F] =
      :@:(d.job, ChildHopLikeHKD.toHop[F](d.innerChild))

    override def fromRelated[F[_], G[_]](r: H[F]#Related[G]): Adult[G] =
      Adult[G](
        job = r.head,
        innerChild = ChildHopLikeHKD.fromRelated[F, G](r.tail)
      )
  }
}

class HKDSpec extends FunSuite {
  test("empty") {
    assert((1 + 1) == 2)
  }
}
