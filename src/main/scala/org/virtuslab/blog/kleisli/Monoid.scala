package org.virtuslab.blog.kleisli

trait Monoid[F] {
  def zero: F
  def append(f1: F, f2: F): F
}

object Monoid {
  implicit object LongMonoid extends Monoid[Long] {
    override def zero: Long = 0L
    override def append(f1: Long, f2: Long): Long = f1 + f2
  }
}
