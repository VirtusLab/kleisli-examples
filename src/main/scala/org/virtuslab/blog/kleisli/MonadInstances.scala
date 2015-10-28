package org.virtuslab.blog.kleisli

import scala.util.{ Success, Try }

trait MonadInstances {
  implicit def eitherMonad[L] = new Monad[Either[L, ?]] {
    override def point[A](a: => A): Either[L, A] = Right(a)

    override def bind[A, B](ma: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] = ma.right.flatMap(f)

    override def fmap[A, B](ma: Either[L, A])(f: (A) => B): Either[L, B] = ma.right.map(f)
  }

  implicit object TryMonad extends Monad[Try] {
    override def point[A](a: => A): Try[A] = Success(a)

    override def bind[A, B](ma: Try[A])(f: (A) => Try[B]): Try[B] = ma flatMap f

    override def fmap[A, B](ma: Try[A])(f: (A) => B): Try[B] = ma map f
  }
}
