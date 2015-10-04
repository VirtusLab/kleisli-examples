package org.virtuslab.blog.kleisli

import scala.language.{ implicitConversions, higherKinds }

trait ReaderTInstances {
  import Arrow._

  abstract class ReaderArrow[Env, =>:[_, _]] extends Arrow[ReaderT[Env, =>:, ?, ?]] {
    implicit def A: Arrow[=>:]

    override def id[A] = arr(identity[A])

    override def arr[A, B](f: (A) => B) = ReaderT(A.arr { case (x, _) => f(x) })

    override def first[A, B, C](f: ReaderT[Env, =>:, A, B]) = {
      val swapsnd: (((A, C), Env)) => ((A, Env), C) = { case ((a, c), e) => ((a, e), c) }
      ReaderT(A.arr(swapsnd) >>> A.first(f.run))
    }

    override def second[A, B, C](f: ReaderT[Env, =>:, A, B]) = {
      val swapfst: (((C, A), Env)) => (C, (A, Env)) = { case ((c, a), e) => (c, (a, e)) }
      ReaderT(A.arr(swapfst) >>> A.second(f.run))
    }

    override def compose[A, B, C](fbc: ReaderT[Env, =>:, B, C], fab: ReaderT[Env, =>:, A, B]) = {
      val dupenv: ((A, Env)) => ((A, Env), Env) = { case ((a, e)) => ((a, e), e) }
      ReaderT(A.arr(dupenv) >>> A.first(fab.run) >>> fbc.run)
    }
  }

  implicit def readerArrow[Env, =>:[_, _]](implicit a: Arrow[=>:]) = new ReaderArrow[Env, =>:] {
    override implicit def A: Arrow[=>:] = a
  }

  final class ReaderTTransformer[Env] extends ArrowTransformer[λ[(α[_, _], β, γ) => ReaderT[Env, α, β, γ]]] {
    override def lift[=>:[_, _]: Arrow, A, B](f: A =>: B) = ReaderT.liftReader(f)
  }

  implicit def readerTTransfromer[Env] = new ReaderTTransformer[Env]
}
