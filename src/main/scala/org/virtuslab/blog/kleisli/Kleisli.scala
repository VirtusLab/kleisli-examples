package org.virtuslab.blog.kleisli

import scala.language.{ implicitConversions, higherKinds }

final case class Kleisli[M[_], A, B](run: A => M[B]) {
  import Monad._
  import Kleisli._

  def apply(a: A) = run(a)

  def >=>[C](k: Kleisli[M, B, C])(implicit m: Monad[M]): Kleisli[M, A, C] = Kleisli((a: A) => this(a) >>= k.run)
  def andThen[C](k: Kleisli[M, B, C])(implicit m: Monad[M]): Kleisli[M, A, C] = this >=> k

  def >==>[C](k: B => M[C])(implicit m: Monad[M]): Kleisli[M, A, C] = this >=> Kleisli(k)
  def andThenK[C](k: B => M[C])(implicit m: Monad[M]): Kleisli[M, A, C] = this >==> k

  def <=<[C](k: Kleisli[M, C, A])(implicit m: Monad[M]): Kleisli[M, C, B] = k >=> this
  def compose[C](k: Kleisli[M, C, A])(implicit m: Monad[M]): Kleisli[M, C, B] = k >=> this

  def <==<[C](k: C => M[A])(implicit m: Monad[M]): Kleisli[M, C, B] = Kleisli(k) >=> this
  def composeK[C](k: C => M[A])(implicit m: Monad[M]): Kleisli[M, C, B] = this <==< k

  def map[C](f: B ⇒ C)(implicit m: Monad[M]): Kleisli[M, A, C] = Kleisli((a: A) => m.fmap(this(a))(f))

  def liftM[N[_]](implicit n: Monad[N]): Kleisli[N, A, M[B]] = Kleisli((a: A) => n.point(this(a)))
  def transform[N[_]](f: M ~> N): Kleisli[N, A, B] = Kleisli(a => f(run(a)))
}

object Kleisli extends KleisliInstances {
  implicit def kleisliFn[M[_], A, B](k: Kleisli[M, A, B]): (A) ⇒ M[B] = k.run

  trait NaturalTransformation[-F[_], +G[_]] {
    def apply[A](fa: F[A]): G[A]
  }
  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
}