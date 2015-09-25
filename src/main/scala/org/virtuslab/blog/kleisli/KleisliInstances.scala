package org.virtuslab.blog.kleisli

import scala.language.higherKinds

trait KleisliInstances {
  //kleisli (a => m b) is arrow
  abstract class KleisliArrow[M[_]] extends Arrow[Kleisli[M, ?, ?]] {
    import Kleisli._
    import Monad._

    implicit def M: Monad[M]

    override def id[A]: Kleisli[M, A, A] = Kleisli(a => M.point(a))

    override def arr[A, B](f: (A) => B): Kleisli[M, A, B] = Kleisli(a => M.point(f(a)))

    override def first[A, B, C](f: Kleisli[M, A, B]): Kleisli[M, (A, C), (B, C)] = Kleisli {
      case (a, c) => f(a) >>= ((b: B) => M.point((b, c)))
    }

    override def second[A, B, C](f: Kleisli[M, A, B]): Kleisli[M, (C, A), (C, B)] = Kleisli {
      case (c, a) => f(a) >>= ((b: B) => M.point((c, b)))
    }

    override def compose[A, B, C](fbc: Kleisli[M, B, C], fab: Kleisli[M, A, B]): Kleisli[M, A, C] = fab >=> fbc
  }

  implicit def kleisliArrow[M[_]](implicit m: Monad[M]) = new KleisliArrow[M] {
    override implicit def M: Monad[M] = m
  }
}
