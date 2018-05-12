package example

import cats.~>
import org.scalatest.FunSuite

object CopSpec {
  final case class Foo[+A](value: A)
  final case class Bar[+A](value: A)
  final case class Baz[+A](value: A)

  object MyLanguage extends Language {
    override type CopType[+X] = :+:[Foo, :+:[Bar, :+:[Baz, CNil[X], X], X], X]
    override type PopType[Z[_]] = :*:[Foo, :*:[Bar, :*:[Baz, PNil[Z], Z], Z], Z]

    def injectFoo[X](foo: Foo[X]): CopType[X] = inject[Foo, X](foo)
    def injectBar[X](bar: Bar[X]): CopType[X] = inject[Bar, X](bar)
    def injectBaz[X](baz: Baz[X]): CopType[X] = inject[Baz, X](baz)

    def selectFoo[X](c: CopType[X]): Option[Foo[X]] = select[Foo, X](c)
    def selectBar[X](c: CopType[X]): Option[Bar[X]] = select[Bar, X](c)
    def selectBaz[X](c: CopType[X]): Option[Baz[X]] = select[Baz, X](c)
  }

  final case class Res[A](value: A)

  final case object FooInterp extends ~>[Foo, Res] {
    override def apply[A](fa: Foo[A]): Res[A] = Res(fa.value)
  }

  final case object BarInterp extends ~>[Bar, Res] {
    override def apply[A](fa: Bar[A]): Res[A] = Res(fa.value)
  }

  final case object BazInterp extends ~>[Baz, Res] {
    override def apply[A](fa: Baz[A]): Res[A] = Res(fa.value)
  }
}

class CopSpec extends FunSuite {
  test("basic") {
    import CopSpec._

    val x = Foo[Int](1)
    val c = MyLanguage.injectFoo(x)
    val p = FooInterp :*: BarInterp :*: BazInterp :*: PNil[Res]()
    val r = MyLanguage.consume[Res, Int](c, p)
    assert(r == Res(1))
  }
}
