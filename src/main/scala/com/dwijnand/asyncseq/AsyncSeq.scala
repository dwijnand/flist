package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Failure, Success }

sealed abstract class AsyncSeq[A] private {
  def next: AsyncSeq[A]

  private[AsyncSeq] val promise = Promise[Option[A]]()
  val future: Future[Option[A]] = promise.future

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

  final def map[B](f: A => B)(implicit ec: ExecutionContext): AsyncSeq[B] =
    AsyncSeq.mapped(this, f)

  final def flatMap[B](f: A => AsyncSeq[B])(implicit ec: ExecutionContext): AsyncSeq[B] =
    ???

  final def toStream: Stream[Future[Option[A]]] = {
    lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(this, stream.map(_.next))
    stream.map(_.future)
  }
}

object AsyncSeq {
  def apply[A](head: Future[A], fetch: A => Option[Future[A]])(implicit ec: ExecutionContext): AsyncSeq[A] = {
    val seed = new Seed(fetch)
    seed.promise tryCompleteWith head.map(Some(_))
    seed
  }

  def mapped[A, B](source: AsyncSeq[A], f: A => B)(implicit ec: ExecutionContext): AsyncSeq[B] = {
    val mapped = new Mapped(source, f)
    mapped.promise tryCompleteWith source.future.map(_ map f)
    mapped
  }

  private final class Seed[A](fetch: A => Option[Future[A]])(implicit ec: ExecutionContext)
    extends AsyncSeq[A]
  {
    lazy val next = new Seed(fetch)

    future.onSuccess {
      case Some(result) =>
        fetch(result) match {
          case Some(nextResult) => next.promise tryCompleteWith nextResult.map(Some(_))
          case None             => next.promise success None
        }
    }
  }
  private final class Mapped[A, B](source: AsyncSeq[A], f: A => B)(implicit ec: ExecutionContext)
    extends AsyncSeq[B]
  {
    lazy val next = new Mapped(source.next, f)

    future.onSuccess {
      case Some(_) => next.promise tryCompleteWith source.next.future.map(_ map f)
    }
  }
}
