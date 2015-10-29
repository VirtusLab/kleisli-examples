package org.virtuslab.blog.kleisli

import scala.language.{ implicitConversions, higherKinds }

final case class WriterT[Out, =>:[_, _], A, B](run: A =>: (B, Out))

object WriterT extends ReaderTInstances {
  import Arrow._

  def liftWriter[Out, =>:[_, _], A, B](f: A =>: B)(implicit arr: Arrow[=>:], m: Monoid[Out]) =
    WriterT(f >>> arr.arr(b => (b, m.zero)))

  implicit def elimWriter[Out, =>:[_, _], A, B](w: WriterT[Out, =>:, A, B]): A =>: (B, Out) = w.run
}

