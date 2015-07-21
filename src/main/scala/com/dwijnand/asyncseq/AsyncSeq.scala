package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success }

object AsyncSeq {
  def apply[A](head: Future[A], next: A => Option[Future[A]])(implicit ec: ExecutionContext) = {
    val asyncSeq = new AsyncSeq[A]

    asyncSeq.promise tryCompleteWith head.map(Some(_))

    asyncSeq.future.onSuccess {
      case Some(result) =>
        next(result) match {
          case Some(nextResult) => asyncSeq.next.promise tryCompleteWith nextResult.map(Some(_))
          case None             => asyncSeq.next.promise success None
        }
    }

    asyncSeq
  }
}
final class AsyncSeq[A] private {
  lazy val next = new AsyncSeq[A]

  private[AsyncSeq] val promise = Promise[Option[A]]()
  val future = promise.future

  @tailrec def isAllCompleted: Boolean =
    future.value match {
      case None                => false
      case Some(Success(None)) => true
      case Some(Success(_))    => next.isAllCompleted
      case Some(Failure(_))    => true
    }

  def toFuture(implicit ec: ExecutionContext): Future[Vector[A]] = {
    def loop(asyncSeq: AsyncSeq[A], acc: Vector[A]): Future[Vector[A]] = {
      asyncSeq.future.flatMap {
        case None    => Future successful acc
        case Some(x) => loop(asyncSeq.next, acc :+ x)
      }
    }
    loop(this, Vector.empty)
  }

  def map[B](f: A => B): AsyncSeq[B] = {
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
