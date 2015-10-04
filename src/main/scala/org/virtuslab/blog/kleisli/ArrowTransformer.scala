package org.virtuslab.blog.kleisli

import scala.language.higherKinds

trait ArrowTransformer[F[_[_, _], _, _]] {
  def lift[=>:[_, _]: Arrow, A, B](arr: A =>: B): F[=>:, A, B]
}

