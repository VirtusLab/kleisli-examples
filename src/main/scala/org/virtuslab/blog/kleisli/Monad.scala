package org.virtuslab.blog.kleisli

import scala.language.{ higherKinds, implicitConversions }

trait Monad[M[_]] {
  def point[A](a: => A): M[A]
  def bind[A, B](ma: M[A])(f: A => M[B]): M[B]
  def fmap[A, B](ma: M[A])(f: A ⇒ B): M[B]
}

final class MonadOps[M[_], A](val self: M[A])(implicit val monad: Monad[M]) {
  def >>=[B](f: A => M[B]) = monad.bind(self)(f)
}

object Monad extends MonadInstances {
  implicit def ToMonadOps[M[_], A](v: M[A])(implicit m: Monad[M]): MonadOps[M, A] = new MonadOps(v)
}
