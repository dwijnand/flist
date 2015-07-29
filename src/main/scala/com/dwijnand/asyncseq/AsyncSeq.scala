package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.util.{ Failure, Success }

sealed abstract class AsyncSeq[A] private {
  private[AsyncSeq] val promise = Promise[Option[A]]()

  val head: Future[Option[A]] = promise.future
  def tail: AsyncSeq[A]
}

object AsyncSeq {
  implicit final class AsyncSeqOps[A](private val asyncSeq: AsyncSeq[A]) extends AnyVal {
    @tailrec def isAllCompleted: Boolean =
      asyncSeq.head.value match {
        case None                => false
        case Some(Success(None)) => true
        case Some(Success(_))    => asyncSeq.tail.isAllCompleted
        case Some(Failure(_))    => true
      }

    def toFuture(implicit ec: EC): Future[Vector[A]] = {
      def loop(asyncSeq: AsyncSeq[A], acc: Vector[A]): Future[Vector[A]] = {
        asyncSeq.head.flatMap {
          case None    => Future successful acc
          case Some(x) => loop(asyncSeq.tail, acc :+ x)
        }
      }
      loop(asyncSeq, Vector.empty)
    }

    def map[B](f: A => B)(implicit ec: EC)                 : AsyncSeq[B] = AsyncSeq.mapped(asyncSeq, f)
    def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC)   : AsyncSeq[B] = AsyncSeq.flatMapped(asyncSeq, f)
    def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]) : AsyncSeq[B] = AsyncSeq.flatten(asyncSeq)

    def toStream: Stream[Future[Option[A]]] = {
      lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(asyncSeq, stream.map(_.tail))
      stream.map(_.head)
    }
  }

  def apply[A](head: Future[A], fetch: A => Option[Future[A]])(implicit ec: EC): AsyncSeq[A] = {
    val seed = new Seed(fetch)
    seed.promise tryCompleteWith head.map(Some(_))
    seed
  }

  def mapped[A, B](source: AsyncSeq[A], f: A => B)(implicit ec: EC): AsyncSeq[B] = {
    val mapped = new Mapped(source, f)
    mapped.promise tryCompleteWith source.head.map(_ map f)
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
    lazy val tail = new Seed(fetch)

    head.onSuccess {
      case Some(result) =>
        fetch(result) match {
          case Some(nextResult) => tail.promise tryCompleteWith nextResult.map(Some(_))
          case None             => tail.promise success None
        }
    }
  }

  private final class Mapped[A, B](source: AsyncSeq[A], f: A => B)(implicit ec: EC) extends AsyncSeq[B] {
    lazy val tail = new Mapped(source.tail, f)

    head.onSuccess {
      case Some(_) => tail.promise tryCompleteWith source.tail.head.map(_ map f)
    }
  }

//  private final class FlatMapped[A, B](source: AsyncSeq[A], f: A => AsyncSeq[B])(implicit ec: EC)
//    extends AsyncSeq[B]
//  {
//    lazy val next = new FlatMapped(source.next, f)
//  }
}
