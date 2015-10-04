package org.virtuslab.blog.kleisli

import scala.language.{ implicitConversions, higherKinds }

final case class ReaderT[Env, =>:[_, _], A, B](run: (A, Env) =>: B) {
  import Arrow._
  def -|(action: (B, Env) => Unit)(implicit A: Arrow[=>:]) = this >>> ReaderT(A.arr { case (b, e) => action(b, e); b })
}

object ReaderT extends ReaderTInstances {
  import Arrow._

  def liftReader[Env, =>:[_, _], A, B](f: A =>: B)(implicit arr: Arrow[=>:]) =
    ReaderT(arr.first[A, B, Env](f) >>> arr.arr(_._1))

  implicit def elimReader[Env, =>:[_, _], A, B](r: ReaderT[Env, =>:, A, B]): (A, Env) =>: B = r.run

  def returnR[Env, =>:[_, _], A, B](implicit arr: Arrow[=>:]) = ReaderT[Env, =>:, A, (A, Env)](arr.id)
}

