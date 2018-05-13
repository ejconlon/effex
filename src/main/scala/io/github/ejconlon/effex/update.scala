package io.github.ejconlon.effex

import cats.effect.IO
import cats.{Monad, Monoid}

import scala.language.{implicitConversions, reflectiveCalls}

// Must follow these laws:
// act(s, empty) == s
// act(act(s, e), f) == act(s, combine(e, f))
trait Action[S, E] {
  val monoid: Monoid[E]
  def act(state: S, effect: E): S
}

object Action {
  def apply[S, E](implicit action: Action[S, E]): Action[S, E] = action

  implicit def fromMonoid[S](implicit monoidS: Monoid[S]): Action[S, S] =
    new Action[S, S] {
      override val monoid: Monoid[S] = monoidS
      override def act(state: S, effect: S): S = monoid.combine(state, effect)
    }
}

sealed trait UpdateCompanion { comp =>
  type Type[S, E, A]

  def acting[S, E](f: S => E)(implicit action: Action[S, E]): Type[S, E, S]

  def actingIO[S, E](f: S => IO[E])(implicit action: Action[S, E]): Type[S, E, S]

  def pure[S, E, A](value: A)(implicit action: Action[S, E]): Type[S, E, A]

  def raiseError[S, E, A](e: Throwable): Type[S, E, A]

  def liftIO[S, E, A](ioa: IO[A])(implicit action: Action[S, E]): Type[S, E, A]

  def contraMap[R, S, E, A](update: Type[S, E, A])(f: R => S): Type[R, E, A]

  def map[S, E, A, B](update: Type[S, E, A])(f: A => B): Type[S, E, B]

  def flatMap[S, E, A, B](update: Type[S, E, A])(f: A => Type[S, E, B])(implicit action: Action[S, E]): Type[S, E, B]

  def run[S, E, A, B](
    update: Type[S, E, A],
    state: S
  )(
    f: A => Type[S, E, Either[A, B]]
  )(
    implicit action: Action[S, E]
  ): IO[(E, B)]

  def tailRecM[S, E, A, B](
    value: A
  )(
    f: A => Type[S, E, Either[A, B]]
  )(
    implicit action: Action[S, E]
  ): Type[S, E, B]

  final class MonadInstance[S, E](implicit action: Action[S, E]) extends Monad[({type F[X] = Type[S, E, X]})#F] {
    override def flatMap[A, B](fa: Type[S, E, A])(f: A => Type[S, E, B]): Type[S, E, B] =
      comp.flatMap[S, E, A, B](fa)(f)

    override def tailRecM[A, B](value: A)(f: A => Type[S, E, Either[A, B]]): Type[S, E, B] =
      comp.tailRecM[S, E, A, B](value)(f)

    override def pure[A](value: A): Type[S, E, A] =
      comp.pure[S, E, A](value)
  }

  implicit def monad[S, E](implicit action: Action[S, E]): Monad[({type F[X] = Type[S, E, X]})#F] = new MonadInstance[S, E]
}

object UpdateIO extends UpdateCompanion { comp =>
  override type Type[S, E, A] = S => IO[(E, A)]

  override def acting[S, E](f: S => E)(implicit action: Action[S, E]): Type[S, E, S] =
    (s: S) => IO {
      val e = f(s)
      (e, action.act(s, e))
    }

  override def actingIO[S, E](f: S => IO[E])(implicit action: Action[S, E]): Type[S, E, S] =
    (s: S) => f(s).map { e => (e, action.act(s, e)) }

  override def pure[S, E, A](value: A)(implicit action: Action[S, E]): Type[S, E, A] =
    (_: S) => IO.pure((action.monoid.empty, value))

  override def raiseError[S, E, A](e: Throwable): Type[S, E, A] =
    (_: S) => IO.raiseError(e)

  override def liftIO[S, E, A](ioa: IO[A])(implicit action: Action[S, E]): Type[S, E, A] =
    (_: S) => ioa.map { (action.monoid.empty, _) }

  override def contraMap[R, S, E, A](update: Type[S, E, A])(f: R => S): Type[R, E, A] =
    (r: R) => update(f(r))

  override def map[S, E, A, B](update: Type[S, E, A])(f: A => B): Type[S, E, B] =
    (s: S) => update(s).map { case (e, a) => (e, f(a)) }

  override def flatMap[S, E, A, B](update: Type[S, E, A])(f: A => Type[S, E, B])(implicit action: Action[S, E]): Type[S, E, B] =
    (s: S) =>
      for {
        x <- update(s)
        (e1, a) = x
        v = f(a)
        t = action.act(s, e1)
        y <- v(t)
        (f1, b) = y
        g1 = action.monoid.combine(e1, f1)
      } yield (g1, b)

  override def run[S, E, A, B](
    update: Type[S, E, A],
    state: S
  )(
    f: A => Type[S, E, Either[A, B]]
  )(implicit action: Action[S, E]): IO[(E, B)] =
    subRun[S, E, A, B](map[S, E, A, Either[A, B]](update) { Left(_) }, state, action.monoid.empty)(f)

  private[this] def subRun[S, E, A, B](
    update: Type[S, E, Either[A, B]],
    state: S,
    history: E
  )(
    f: A => Type[S, E, Either[A, B]]
  )(
    implicit action: Action[S, E]
  ): IO[(E, B)] =
    update(state).flatMap { case (effect, eab) =>
      val newHistory = action.monoid.combine(history, effect)
      eab match {
        case Left(a) =>
          IO.suspend[(E, B)] {
            val newState = action.act(state, effect)
            val newUpdate = f(a)
            subRun[S, E, A, B](newUpdate, newState, newHistory)(f)
          }
        case Right(b) => IO.pure((newHistory, b))
      }
    }

  override def tailRecM[S, E, A, B](value: A)(f: A => Type[S, E, Either[A, B]])(implicit action: Action[S, E]): Type[S, E, B] =
    (s: S) => run[S, E, A, B](pure[S, E, A](value), s)(f)

  final class Ops[S, E, A](val self: Type[S, E, A]) extends AnyVal {
    def map[B](f: A => B): Type[S, E, B] = comp.map(self)(f)
    def flatMap[B](f: A => Type[S, E, B])(implicit action: Action[S, E]): Type[S, E, B] = comp.flatMap(self)(f)
  }

  implicit def ops[S, E, A](target: Type[S, E, A]): Ops[S, E, A] = new Ops[S, E, A](target)
}

sealed trait UpdateCodIO[S, E, A] { self =>
  import UpdateCodIO.subRun

  def apply[Z](state: S)(handle: (E, A) => IO[Z]): IO[Z]

  final def contraMap[R](f: R => S): UpdateCodIO[R, E, A] =
    new UpdateCodIO[R, E, A] {
      override def apply[Z](state: R)(handle: (E, A) => IO[Z]): IO[Z] =
        self.apply(f(state))(handle)
    }

  final def map[B](f: A => B): UpdateCodIO[S, E, B] =
    new UpdateCodIO[S, E, B] {
      override def apply[Z](state: S)(handle: (E, B) => IO[Z]): IO[Z] =
        self.apply(state) { (e, a) => handle(e, f(a)) }
    }

  final def flatMap[B](f: A => UpdateCodIO[S, E, B])(implicit action: Action[S, E]): UpdateCodIO[S, E, B] =
    new UpdateCodIO[S, E, B] {
      override def apply[Z](state: S)(handle: (E, B) => IO[Z]): IO[Z] =
        self.apply[Z](state) { (e1, a) =>
          val newState = action.act(state, e1)
          val newUpdate = f(a)
          newUpdate.apply[Z](newState) { (f1, b) =>
            val g1 = action.monoid.combine(e1, f1)
            handle(g1, b)
          }
        }
    }

  final def run[B](state: S)(f: A => UpdateCodIO[S, E, Either[A, B]])(implicit action: Action[S, E]): IO[(E, B)] =
    subRun[S, E, A, B](map[Either[A, B]] { Left(_) }, state, action.monoid.empty)(f)
}

object UpdateCodIO extends UpdateCompanion { comp =>
  override type Type[S, E, A] = UpdateCodIO[S, E, A]

  override def acting[S, E](f: S => E)
    (implicit action: Action[S, E]): Type[S, E, S] =
    new UpdateCodIO[S, E, S] {
      override def apply[Z](state: S)(handle: (E, S) => IO[Z]): IO[Z] = {
        val e = f(state)
        val newState = action.act(state, e)
        handle(e, newState)
      }
    }

  override def actingIO[S, E](f: S => IO[E])
    (implicit action: Action[S, E]): Type[S, E, S] =
    new UpdateCodIO[S, E, S] {
      override def apply[Z](state: S)(handle: (E, S) => IO[Z]): IO[Z] = {
        f(state).flatMap { e =>
          val newState = action.act(state, e)
          handle(e, newState)
        }
      }
    }

  override def pure[S, E, A](value: A)
    (implicit action: Action[S, E]): Type[S, E, A] =
    new UpdateCodIO[S, E, A] {
      override def apply[Z](state: S)(handle: (E, A) => IO[Z]): IO[Z] =
        handle(action.monoid.empty, value)
    }

  override def raiseError[S, E, A](e: Throwable): Type[S, E, A] =
    new UpdateCodIO[S, E, A] {
      override def apply[Z](state: S)(handle: (E, A) => IO[Z]): IO[Z] =
        IO.raiseError(e)
    }

  override def liftIO[S, E, A](ioa: IO[A])
    (implicit action: Action[S, E]): Type[S, E, A] =
    new UpdateCodIO[S, E, A] {
      override def apply[Z](state: S)(handle: (E, A) => IO[Z]): IO[Z] =
        ioa.flatMap { a =>
          handle(action.monoid.empty, a)
        }
    }

  override def contraMap[R, S, E, A](update: Type[S, E, A])
    (f: R => S): Type[R, E, A] = update.contraMap(f)

  override def map[S, E, A, B](update: Type[S, E, A])
    (f: A => B): Type[S, E, B] = update.map(f)

  override def flatMap[S, E, A, B](update: Type[S, E, A])
    (f: A => Type[S, E, B])
    (implicit action: Action[S, E]): Type[S, E, B] = update.flatMap(f)

  override def run[S, E, A, B](update: Type[S, E, A], state: S)
    (f: A => Type[S, E, Either[A, B]])
    (implicit action: Action[S, E]): IO[(E, B)] = update.run(state)(f)

  private def subRun[S, E, A, B](
    update: Type[S, E, Either[A, B]],
    state: S,
    history: E
  )(
    f: A => Type[S, E, Either[A, B]]
  )(
    implicit action: Action[S, E]
  ): IO[(E, B)] =
    update.apply[(E, B)](state) { (effect, eab) =>
      val newHistory = action.monoid.combine(history, effect)
      eab match {
        case Left(a) =>
          IO.suspend[(E, B)] {
            val newState = action.act(state, effect)
            val newUpdate = f(a)
            subRun[S, E, A, B](newUpdate, newState, newHistory)(f)
          }
        case Right(b) => IO.pure((newHistory, b))
      }
    }

  override def tailRecM[S, E, A, B](value: A)
    (f: A => Type[S, E, Either[A, B]])
    (implicit action: Action[S, E]): Type[S, E, B] =
    new UpdateCodIO[S, E, B] {
      override def apply[Z](state: S)(handle: (E, B) => IO[Z]): IO[Z] =
        pure[S, E, A](value).run[B](state)(f).flatMap[Z] { case (e, b) => handle(e, b) }
    }
}