package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.util.{ Failure, Success }

sealed abstract class AsyncSeq[A] private {
  def next: AsyncSeq[A]

  private[AsyncSeq] val promise = Promise[Option[A]]()
  val future: Future[Option[A]] = promise.future
}

object AsyncSeq {
  implicit class AsyncSeqOps[A](private val asyncSeq: AsyncSeq[A]) extends AnyVal {
    @tailrec final def isAllCompleted: Boolean =
      asyncSeq.future.value match {
        case None                => false
        case Some(Success(None)) => true
        case Some(Success(_))    => asyncSeq.next.isAllCompleted
        case Some(Failure(_))    => true
      }

    final def toFuture(implicit ec: EC): Future[Vector[A]] = {
      def loop(asyncSeq: AsyncSeq[A], acc: Vector[A]): Future[Vector[A]] = {
        asyncSeq.future.flatMap {
          case None    => Future successful acc
          case Some(x) => loop(asyncSeq.next, acc :+ x)
        }
      }
      loop(asyncSeq, Vector.empty)
    }

    final def map[B](f: A => B)(implicit ec: EC)                : AsyncSeq[B] = AsyncSeq.mapped(asyncSeq, f)
    final def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC)  : AsyncSeq[B] = AsyncSeq.flatMapped(asyncSeq, f)
    final def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B] = AsyncSeq.flatten(asyncSeq)

    final def toStream: Stream[Future[Option[A]]] = {
      lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(asyncSeq, stream.map(_.next))
      stream.map(_.future)
    }
  }

  def apply[A](head: Future[A], fetch: A => Option[Future[A]])(implicit ec: EC): AsyncSeq[A] = {
    val seed = new Seed(fetch)
    seed.promise tryCompleteWith head.map(Some(_))
    seed
  }

  def mapped[A, B](source: AsyncSeq[A], f: A => B)(implicit ec: EC): AsyncSeq[B] = {
    val mapped = new Mapped(source, f)
    mapped.promise tryCompleteWith source.future.map(_ map f)
    mapped
  }

  def flatMapped[A, B](source: AsyncSeq[A], f: A => AsyncSeq[B])(implicit ec: EC): AsyncSeq[B] = {
//    val flatMapped = new FlatMapped(source, f)
//    flatMapped.promise
//    flatMapped
    ???
  }

  def flatten[A, B](source: AsyncSeq[A])(implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B] = {
    // source flatMap ev
    ???
  }

  private final class Seed[A](fetch: A => Option[Future[A]])(implicit ec: EC) extends AsyncSeq[A] {
    lazy val next = new Seed(fetch)

    future.onSuccess {
      case Some(result) =>
        fetch(result) match {
          case Some(nextResult) => next.promise tryCompleteWith nextResult.map(Some(_))
          case None             => next.promise success None
        }
    }
  }

  private final class Mapped[A, B](source: AsyncSeq[A], f: A => B)(implicit ec: EC) extends AsyncSeq[B] {
    lazy val next = new Mapped(source.next, f)

    future.onSuccess {
      case Some(_) => next.promise tryCompleteWith source.next.future.map(_ map f)
    }
  }

//  private final class FlatMapped[A, B](source: AsyncSeq[A], f: A => AsyncSeq[B])(implicit ec: EC)
//    extends AsyncSeq[B]
//  {
//    lazy val next = new FlatMapped(source.next, f)
//  }
}
