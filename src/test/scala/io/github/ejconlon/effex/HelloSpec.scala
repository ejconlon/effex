package io.github.ejconlon.effex

import cats.effect.IO
import org.scalatest.FunSuite

object HelloSpec {
  trait KVStore[F[_], K, V] {
    def get(key: K): F[Option[V]]
    def set(key: K, value: V): F[Unit]
  }

  object KVStore {
    sealed trait DSL[K, V, A] {
      def apply[F[_]](implicit iface: KVStore[F, K, V]): F[A]
    }

    object DSL {
      def get[K, V](key: K): DSL[K, V, Option[V]] =
        new DSL[K, V, Option[V]] {
          override def apply[F[_]](implicit iface: KVStore[F, K, V]): F[Option[V]] =
            iface.get(key)
        }

       def set[K, V](key: K, value: V): DSL[K, V, Unit] =
        new DSL[K, V, Unit] {
          override def apply[F[_]](implicit iface: KVStore[F, K, V]): F[Unit] =
            iface.set(key, value)
        }
    }

    sealed trait Term[K, V, A] extends Product with Serializable

    object Term {
      final case class Get[K, V](key: K) extends Term[K, V, Option[V]]
      final case class Set[K, V](key: K, value: V) extends Term[K, V, Unit]
    }

    def fromState[F[_], K, V](state: State[F, Map[K, V]]): KVStore[F, K, V] =
      new KVStore[F, K, V] {
        override def get(key: K): F[Option[V]] = state.gets { _.get(key) }
        override def set(key: K, value: V): F[Unit] = state.modify { _ + (key -> value) }
      }

    def io[K, V]: IO[KVStore[IO, K, V]] =
      State.io(Map.empty[K, V]).map(fromState)
  }
}

class HelloSpec extends FunSuite {
  test("empty") {
    assert((1 + 1) == 2)
  }
}
