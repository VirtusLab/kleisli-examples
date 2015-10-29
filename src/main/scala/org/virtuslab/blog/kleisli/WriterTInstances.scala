package org.virtuslab.blog.kleisli

import scala.language.{ implicitConversions, higherKinds }

trait WriterTInstances {
  import Arrow._

  abstract class WriterArrow[Out, =>:[_, _]] extends Arrow[WriterT[Out, =>:, ?, ?]] {
    implicit def A: Arrow[=>:]
    implicit def M: Monoid[Out]

    override def id[A] = arr(identity[A])

    override def arr[A, B](f: (A) => B) = WriterT(A.arr(a => (f(a), M.zero)))

    override def first[A, B, C](f: WriterT[Out, =>:, A, B]) = {
      val rstrength: (((B, Out), C)) => ((B, C), Out) = { case ((b, w), c) => ((b, c), w) }
      WriterT(A.first(f.run) >>> A.arr(rstrength))
    }

    override def second[A, B, C](f: WriterT[Out, =>:, A, B]) = {
      val lstrength: ((C, (B, Out))) => ((C, B), Out) = { case (c, (b, w)) => ((c, b), w) }
      WriterT(A.second(f.run) >>> A.arr(lstrength))
    }

    override def compose[A, B, C](fbc: WriterT[Out, =>:, B, C], fab: WriterT[Out, =>:, A, B]) = {
      val join: (((C, Out), Out)) => ((C, Out)) = { case ((c, w2), w1) => (c, M.append(w1, w2)) }
      WriterT(fab.run >>> A.first(fbc.run) >>> A.arr(join))
    }
  }

  implicit def writerArrow[Out, =>:[_, _]](implicit a: Arrow[=>:], m: Monoid[Out]) = new WriterArrow[Out, =>:] {
    override implicit def A: Arrow[=>:] = a
    override implicit def M: Monoid[Out] = m
  }

  final class WriterTTransformer[Out: Monoid] extends ArrowTransformer[λ[(α[_, _], β, γ) => WriterT[Out, α, β, γ]]] {
    override def lift[=>:[_, _]: Arrow, A, B](f: A =>: B) = WriterT.liftWriter(f)
  }

  implicit def writerTTransfromer[Out: Monoid] = new WriterTTransformer[Out]
}
