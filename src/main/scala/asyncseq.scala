package asyncseq

import scala.concurrent.{ ExecutionContext => EC, Future }

sealed abstract class AsyncSeq[A] {
  def map[B](f: A => B)(implicit ec: EC): AsyncSeq[B] =
    this match {
      case Last(a)     => Last(f(a))
      case Cons(a, ft) => Cons(f(a), ft map (_ map f))
    }

  def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC): AsyncSeq[B] =
    this match {
      case Last(a)     => f(a)
      case Cons(a, ft) => f(a) concat (ft map (_ flatMap f))
    }

  def concat(rhs: Future[AsyncSeq[A]])(implicit ec: EC): AsyncSeq[A] =
    this match {
      case Last(a)     => Cons(a, rhs)
      case Cons(a, ft) => Cons(a, ft map (_ concat rhs))
    }
}

// TODO: AsyncCons? Async.Cons? Cons?
final case class Cons[A](value: A, tail: Future[AsyncSeq[A]]) extends AsyncSeq[A]
final case class Last[A](value: A)                            extends AsyncSeq[A]

object AsyncSeq {
  def apply[A](start: A, call: A => Option[Future[A]])(implicit ec: EC): AsyncSeq[A] =
    call(start) match {
      case Some(fa) => Cons(start, fa.map(a => AsyncSeq(a, call)))
      case None     => Last(start)
    }
}
