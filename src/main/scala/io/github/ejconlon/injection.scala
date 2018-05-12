package io.github.ejconlon

import scala.util.Try

/**
  * Think of B as a "bigger type" than A; that B "contains" A.
  * We can always take an A and make it a B, but can can't always take a B and make in an A.
  */
trait Injection[A, B] {
  def apply(a: A): B
  def invert(b: B): Try[A]
}
