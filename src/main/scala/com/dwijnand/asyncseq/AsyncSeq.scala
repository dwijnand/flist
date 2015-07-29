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
  implicit final class AsyncSeqOps[A](private val xs: AsyncSeq[A]) extends AnyVal {
    @tailrec def isAllCompleted: Boolean =
      xs.head.value match {
        case None                => false
        case Some(Success(None)) => true
        case Some(Success(_))    => xs.tail.isAllCompleted
        case Some(Failure(_))    => true
      }

    def toFuture(implicit ec: EC): Future[Vector[A]] = {
      def loop(asyncSeq: AsyncSeq[A], acc: Vector[A]): Future[Vector[A]] = {
        asyncSeq.head.flatMap {
          case None    => Future successful acc
          case Some(x) => loop(asyncSeq.tail, acc :+ x)
        }
      }
      loop(xs, Vector.empty)
    }

    def map[B](f: A => B)(implicit ec: EC)                 : AsyncSeq[B] = AsyncSeq.map(xs, f)
    def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC)   : AsyncSeq[B] = AsyncSeq.flatMap(xs, f)
    def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]) : AsyncSeq[B] = AsyncSeq.flatten(xs)

    def toStream: Stream[Future[Option[A]]] = {
      lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(xs, stream.map(_.tail))
      stream.map(_.head)
    }
  }

  def apply[A](head: Future[A], fetch: A => Option[Future[A]])(implicit ec: EC): AsyncSeq[A] = {
    val seed = new Seed(fetch)
    seed.promise tryCompleteWith head.map(Some(_))
    seed
  }

  def map[A, B](xs: AsyncSeq[A], f: A => B)(implicit ec: EC): AsyncSeq[B] = {
    val mapped = new Mapped(xs, f)
    mapped.promise tryCompleteWith xs.head.map(_ map f)
    mapped
  }

  def flatMap[A, B](xs: AsyncSeq[A], f: A => AsyncSeq[B])(implicit ec: EC): AsyncSeq[B] = {
//    val flatMapped = new FlatMapped(xs, f)
//    flatMapped.promise
//    flatMapped
    ???
  }

  def flatten[A, B](xs: AsyncSeq[A])(implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B] = {
    // xs flatMap ev
    ???
  }

  final class Seed[A] private[AsyncSeq] (fetch: A => Option[Future[A]])(implicit ec: EC) extends AsyncSeq[A] {
    lazy val tail = new Seed(fetch)

    head.onSuccess {
      case Some(result) =>
        fetch(result) match {
          case Some(nextResult) => tail.promise tryCompleteWith nextResult.map(Some(_))
          case None             => tail.promise success None
        }
    }
  }

  final class Mapped[A, B] private[AsyncSeq] (xs: AsyncSeq[A], f: A => B)(implicit ec: EC) extends AsyncSeq[B] {
    lazy val tail = new Mapped(xs.tail, f)

    head.onSuccess {
      case Some(_) => tail.promise tryCompleteWith xs.tail.head.map(_ map f)
    }
  }

//  final class FlatMapped[A, B] private[AsyncSeq] (xs: AsyncSeq[A], f: A => AsyncSeq[B])(implicit ec: EC)
//    extends AsyncSeq[B]
//  {
//    lazy val tail = new FlatMapped(xs.tail, f)
//  }
}
