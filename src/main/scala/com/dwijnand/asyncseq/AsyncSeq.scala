package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success }

object AsyncSeq {
  def apply[A](head: Future[A], fetch: A => Option[Future[A]])(implicit ec: ExecutionContext) = {
    val asyncSeq = new AsyncSeq(fetch)
    asyncSeq.promise tryCompleteWith head.map(Some(_))
    asyncSeq
  }
}
final class AsyncSeq[A] private (fetch: A => Option[Future[A]])(implicit ec: ExecutionContext) {
  lazy val next = new AsyncSeq(fetch)

  private[AsyncSeq] val promise = Promise[Option[A]]()
  val future = promise.future

  future.onSuccess {
    case Some(result) =>
      fetch(result) match {
        case Some(nextResult) => next.promise tryCompleteWith nextResult.map(Some(_))
        case None             => next.promise success None
      }
  }

  @tailrec def isAllCompleted: Boolean =
    future.value match {
      case None                => false
      case Some(Success(None)) => true
      case Some(Success(_))    => next.isAllCompleted
      case Some(Failure(_))    => true
    }

  def toFuture: Future[Vector[A]] = {
    def loop(asyncSeq: AsyncSeq[A], acc: Vector[A]): Future[Vector[A]] = {
      asyncSeq.future.flatMap {
        case None    => Future successful acc
        case Some(x) => loop(asyncSeq.next, acc :+ x)
      }
    }
    loop(this, Vector.empty)
  }

  def map[B](f: A => B): AsyncSeq[B] = {
//    val asyncSeq = new AsyncSeq[B]
//    asyncSeq.promise tryCompleteWith (future map (_ map f))
//    asyncSeq.future.onSuccess {
//      case Some(result) =>
//    }
//    asyncSeq
    ???
  }

  def flatMap[B](f: A => AsyncSeq[B]): AsyncSeq[B] = {
    ???
  }

  def toStream: Stream[Future[Option[A]]] = {
    lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(this, stream.map(_.next))
    stream.map(_.future)
  }
}
