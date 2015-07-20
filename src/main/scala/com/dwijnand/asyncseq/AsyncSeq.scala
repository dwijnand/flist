package com.dwijnand.asyncseq

import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success }

// TODO: Create typeclass for these functions & types
// TODO: Move types in?
// TODO: Seed, Mapped, FlatMapped, OptMapped? optMap? contraOptMap? OptSeed? VectorSeed?
// TODO: Move ops to ops class
object AsyncSeq {
  def apply[A](head: Future[A], tail: A => Option[Future[A]])(implicit ec: ExecutionContext) = {
    val asyncSeq = new AsyncSeq(head, tail)
    head.map(Some(_)) onComplete asyncSeq.promise.tryComplete
    asyncSeq
  }
}
class AsyncSeq[A] private (head: Future[A], tail: A => Option[Future[A]])(implicit ec: ExecutionContext) {
  lazy val next = new AsyncSeq(head, tail)

  private[AsyncSeq] val promise = Promise[Option[A]]()
  val future = promise.future

  future.onSuccess {
    case Some(result) =>
      tail(result) match {
        case Some(nextResult) => nextResult.map(Some(_)) onComplete next.promise.tryComplete
        case None             => next.promise success None
      }
  }

  def isAllCompleted: Boolean =
    future.value match {
      case None                            => false
      case Some(Success(xs)) if xs.isEmpty => true
      case Some(Success(xs))               => next.isAllCompleted
      case Some(Failure(_))                => true
    }

  def unpaginate(implicit ec: ExecutionContext): Future[Vector[A]] = {
    def loop(p: AsyncSeq[A], acc: Vector[A]): Future[Vector[A]] = {
      p.future.flatMap {
        case xs if xs.isEmpty => Future successful acc
        case ys               => loop(p.next, acc ++ ys)
      }
    }
    loop(this, Vector.empty)
  }

  def map[B](f: A => B): AsyncSeq[B] = {
    ???
  }

  def toStream: Stream[Future[Option[A]]] = {
    lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(this, stream.map(_.next))
    stream.map(_.future)
  }
}
