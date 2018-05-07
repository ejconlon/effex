package example

import cats.effect.IO

import scala.language.implicitConversions

/**
  * A Bracket handles acquiring and releasing resources.
  * It accepts some info parameter (of type `X`) and uses that
  * to acquire a resource (of type `R`) and run some computation.
  * It just so happens that you can use Bracket[X, ?] in for-comprehensions
  * (though you will need to `import Bracket.ops`).
  * This is a Kleisli arrow over CodIO.
  */
object Bracket {
  type Type[X, R] = X => CodIO[R]

  /**
    * The most common real-world way for you to manage a resource.
    * Provide some function to `acquire` the resource, and one to `release` it.
    * `IO.bracket` ensures we always release.
    */
  def apply[X, R](acquire: X => IO[R])(release: R => IO[Unit]): Type[X, R] =
    CodIO.bracket(acquire)(release)

  def const[X, R](resource: R): Type[X, R] =
    new Type[X, R] {
      override def apply(info: X): CodIO[R] =
        CodIO.pure(resource)
    }

  def id[X]: Type[X, X] =
    new Type[X, X] {
      override def apply(info: X): CodIO[X] = CodIO.pure(info)
    }

  final class Ops[X, R](val self: Type[X, R]) extends AnyVal {
    def andThenBracket[S](inner: Type[R, S]): Type[X, S] =
      new Type[X, S] {
        override def apply(info: X): CodIO[S] =
          self.apply(info).flatMap(inner)
      }

    def composeBracket[Z](outer: Type[Z, X]): Type[Z, R] =
      new Ops(outer).andThenBracket(self)

    def passthrough: Type[X, (X, R)] =
      new Type[X, (X, R)] {
        override def apply(info: X): CodIO[(X, R)] =
          self.apply(info).map { (info, _) }
      }

    def contraMap[W](f: W => X): Type[W, R] =
      f.andThen(self.apply)

    def contraMapIO[W](f: W => IO[X]): Type[W, R] =
      new Type[W, R] {
        override def apply(info: W): CodIO[R] =
          CodIO.liftIO(f(info)).flatMap(self.apply)
      }

    def map[S](f: R => S): Type[X, S] =
      new Type[X, S] {
        override def apply(info: X): CodIO[S] =
          self.apply(info).map(f)
      }

    def mapIO[S](f: R => IO[S]): Type[X, S] =
      new Type[X, S] {
        override def apply(info: X): CodIO[S] =
          self.apply(info).mapIO(f)
      }

    def flatMap[S](f: R => Type[X, S]): Type[X, S] =
      new Type[X, S] {
        override def apply(info: X): CodIO[S] =
          new CodIO[S] {
            override def apply[Z](act: S => IO[Z]): IO[Z] =
              self.apply(info) { f(_).apply(info)(act) }
          }
      }
  }

  implicit def ops[X, R](target: Type[X, R]): Ops[X, R] = new Ops(target)
}
