package org.virtuslab.blog.kleisli

trait MonadInstances {
  implicit def eitherMonad[L] = new Monad[({ type λ[β] = Either[L, β] })#λ] {
    override def point[A](a: => A): Either[L, A] = Right(a)

    override def bind[A, B](ma: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] = ma.right.flatMap(f)

    override def fmap[A, B](ma: Either[L, A])(f: (A) => B): Either[L, B] = ma.right.map(f)
  }
}
