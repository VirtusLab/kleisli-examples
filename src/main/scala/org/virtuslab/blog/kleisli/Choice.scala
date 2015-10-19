package org.virtuslab.blog.kleisli

import scala.language.{ implicitConversions, higherKinds }

trait Choice[=>:[_, _]] { self: Arrow[=>:] =>
  def left[A, B, C](f: A =>: B): Either[A, C] =>: Either[B, C]
  def right[A, B, C](f: A =>: B): Either[C, A] =>: Either[C, B]

  def multiplex[A, B, C, D](f: A =>: B, g: C =>: D): Either[A, C] =>: Either[B, D] = compose(left(f), right(g))

  def fanin[A, B, C](f: A =>: C, g: B =>: C): Either[A, B] =>: C = {
    val untag: Either[C, C] => C = {
      case Left(x)  => x
      case Right(y) => y
    }
    compose(arr(untag), multiplex(f, g))
  }
}

final class ChoiceOps[=>:[_, _], A, B](val self: A =>: B)(implicit val choice: Choice[=>:]) {
  def +++[C, D](g: C =>: D): Either[A, C] =>: Either[B, D] = choice.multiplex(self, g)
  def |||[C](g: C =>: B): Either[A, C] =>: B = choice.fanin(self, g)
}

object Choice {
  implicit def ToChoiceOps[F[_, _]: Choice, A, B](v: F[A, B]): ChoiceOps[F, A, B] = new ChoiceOps(v)
}
