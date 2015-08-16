package org.virtuslab.blog.kleisli

trait ArrowInstances {
  // function is arrow
  implicit object FunctionArrow extends Arrow[Function1] {
    override def id[A]: A => A = identity[A]

    override def arr[A, B](f: (A) => B): A => B = f

    override def compose[A, B, C](fbc: B => C, fab: A => B): A => C = fbc compose fab

    override def first[A, B, C](f: A => B): ((A, C)) => (B, C) = prod => (f(prod._1), prod._2)

    override def second[A, B, C](f: A => B): ((C, A)) => (C, B) = prod => (prod._1, f(prod._2))

    override def merge[A, B, C, D](f: (A) => B, g: (C) => D): ((A, C)) => (B, D) = { case (x, y) => (f(x), g(y)) }
  }
}
