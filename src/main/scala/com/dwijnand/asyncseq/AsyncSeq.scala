package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success }

sealed abstract class AsyncSeq[A] private {
  def next: AsyncSeq[A]
  def future: Future[Option[A]]

  @tailrec final def isAllCompleted: Boolean =
    future.value match {
      case None                => false
      case Some(Success(None)) => true
      case Some(Success(_))    => next.isAllCompleted
      case Some(Failure(_))    => true
    }

  final def toFuture(implicit ec: ExecutionContext): Future[Vector[A]] = {
    def loop(asyncSeq: AsyncSeq[A], acc: Vector[A]): Future[Vector[A]] = {
      asyncSeq.future.flatMap {
        case None    => Future successful acc
        case Some(x) => loop(asyncSeq.next, acc :+ x)
      }
    }
    loop(this, Vector.empty)
  }

  final def map[B](f: A => B)(implicit ec: ExecutionContext): AsyncSeq[B] = {
//    val asyncSeq = new AsyncSeq[B]
//    asyncSeq.promise tryCompleteWith (future map (_ map f))
//    asyncSeq.future.onSuccess {
//      case Some(result) =>
//    }
//    asyncSeq
    ???
  }

  final def flatMap[B](f: A => AsyncSeq[B])(implicit ec: ExecutionContext): AsyncSeq[B] = {
    ???
  }

  final def toStream: Stream[Future[Option[A]]] = {
    lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(this, stream.map(_.next))
    stream.map(_.future)
  }
}
object AsyncSeq {
  def apply[A](head: Future[A], fetch: A => Option[Future[A]])(implicit ec: ExecutionContext) = {
    val seed = new Seed(fetch)
    seed.promise tryCompleteWith head.map(Some(_))
    seed
  }

  final class Seed[A] private[AsyncSeq] (fetch: A => Option[Future[A]])(implicit ec: ExecutionContext)
    extends AsyncSeq[A]
  {
    lazy val next = new Seed(fetch)

    private[AsyncSeq] val promise = Promise[Option[A]]()
    val future = promise.future

    future.onSuccess {
      case Some(result) =>
        fetch(result) match {
          case Some(nextResult) => next.promise tryCompleteWith nextResult.map(Some(_))
          case None             => next.promise success None
        }
    }
  }
}
