package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.{ CanBuildFrom => CBF }
import scala.collection.mutable
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.util.{ Failure, Success }

final class AsyncSeq[+A] private (private[AsyncSeq] val promise: Promise[Option[A @uV]], tl: => AsyncSeq[A]) {
  def this() = this(Promise[Option[A]](), new AsyncSeq[A])

  val head: Future[Option[A]] = promise.future

  lazy val tail: AsyncSeq[A] = tl

  // Maps
  def map[B](f: A => B)(implicit ec: EC): AsyncSeq[B] = {
    def loop(xs: AsyncSeq[B], xs1: AsyncSeq[A]): AsyncSeq[B] = {
      xs.promise tryCompleteWith xs1.head.map(_ map f)
      xs.head onSuccess {
        case Some(_) => loop(xs.tail, xs1.tail)
      }
      xs
    }
    loop(new AsyncSeq[B], this)
  }

  def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC) : AsyncSeq[B] = map(f).flatten

  // Other iterators
  def grouped(size: Int)(implicit ec: EC): AsyncSeq[AsyncSeq[A]] = {
    def loop0(xs0: AsyncSeq[A], grouped: AsyncSeq[AsyncSeq[A]]): AsyncSeq[AsyncSeq[A]] = {
      val xs = new AsyncSeq[A]
      loop(xs0, xs, xs, size, grouped)
    }
    def loop(xs0: AsyncSeq[A], xs: AsyncSeq[A], wip: AsyncSeq[A], n: Int, grouped: AsyncSeq[AsyncSeq[A]])
    : AsyncSeq[AsyncSeq[A]] = {
      if (n > 0) {
        xs0.head onComplete {
          case Success(Some(x)) =>
            wip.promise success Some(x)
            loop(xs0.tail, xs, wip.tail, n - 1, grouped)
          case Success(None)    =>
            if (xs eq wip)
              grouped.promise success None
            else {
              wip.promise success None
              grouped.promise success Some(xs)
              grouped.tail.promise success None
            }
          case Failure(t)       =>
            if (xs eq wip)
              grouped.promise failure t
            else {
              wip.promise failure t
              grouped.promise success Some(xs)
              grouped.tail.promise failure t // TODO: a failed seq grouped is failed? or contains a failed seq?
            }
        }
      } else {
        wip.promise success None
        grouped.promise success Some(xs)
        xs0.head onComplete {
          case Success(Some(_)) => loop0(xs0, grouped.tail)
          case Success(None)    => grouped.tail.promise success None
          case Failure(t)       => grouped.tail.promise failure t
        }
      }
      grouped
    }
    loop0(this, new AsyncSeq[AsyncSeq[A]])
  }

  def foldLeft[B](z: B)(op: (B, A) => B)(implicit ec: EC): Future[B] = {
    head flatMap {
      case Some(x) => tail.foldLeft(op(z, x))(op)
      case None    => Future successful z
    }
  }

  def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B] = {
    def loopSeqOfSeq(xs: AsyncSeq[B], xss1: AsyncSeq[A]): AsyncSeq[B] = {
      xss1.head onComplete {
        case Success(Some(xs1)) => loopSeq(xs, xs1, xss1)
        case Success(None)      => xs.promise success None
        case Failure(t)         => xs.promise failure t
      }
      xs
    }

    def loopSeq(xs: AsyncSeq[B], xs1: AsyncSeq[B], xss1: AsyncSeq[A]): Unit = {
      xs1.head onComplete {
        case Success(Some(x)) => xs.promise success Some(x) ; loopSeq(xs.tail, xs1.tail, xss1)
        case Success(None)    => loopSeqOfSeq(xs, xss1.tail)
        case Failure(t)       => xs.promise failure t
      }
    }

    loopSeqOfSeq(new AsyncSeq[B], this)
  }

  // Conversions
  private def fromBuilder[A1 >: A, CC[_]](b: mutable.Builder[A1, CC[A1]])(implicit ec: EC): Future[CC[A1]] =
    foldLeft(b)(_ += _).map(_.result)

  def to[Col[_]](implicit cbf: CBF[Nothing, A, Col[A @uV]], ec: EC): Future[Col[A @uV]] = fromBuilder(cbf())

  def toVector(implicit ec: EC): Future[Vector[A]] = to[Vector]

  def toMap[K, V](implicit ec: EC, ev: A <:< (K, V)): Future[Map[K, V]] =
    foldLeft(Map.newBuilder[K, V])(_ += _).map(_.result)

  // Strings
//  def mkString(start: String, sep: String, end: String): String =
//    addString(new StringBuilder(), start, sep, end).toString
//
//  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
//    @tailrec def loop(xs: AsyncSeq[A], first: Boolean): Unit = {
//      xs.head.value match {
//        case None                   => if (!first) b append sep; b append '?'
//        case Some(Success(None))    =>
//        case Some(Success(Some(x))) => if (!first) b append sep; b append x ; loop(xs.tail, first = false)
//        case Some(Failure(t))       => if (!first) b append sep; b append s"[ex: $t]"
//      }
//    }
//
//    b append start
//    loop(this, first = true)
//    b append end
//    b
//  }
//
//  override def toString = this.mkString("AsyncSeq(", ", ", ")")
}

object AsyncSeq {
  def empty[A]: AsyncSeq[A] = apply()

  def apply[A](seq: A*): AsyncSeq[A] = fromSeq(seq)

  def fromSeq[A](seq: Seq[A]): AsyncSeq[A] = {
    @tailrec def loop(xs: AsyncSeq[A], list: List[A]): Unit =
      list match {
        case h :: t => xs.promise success Some(list.head) ; loop(xs.tail, t)
        case Nil    => xs.promise success None
      }
    val xs = new AsyncSeq[A]
    loop(xs, seq.toList)
    xs
  }

  def unpaginate[A](head: Future[A])(fetch: A => Option[Future[A]])(implicit ec: EC): AsyncSeq[A] = {
    def loop(xs: AsyncSeq[A], f: Future[A]): AsyncSeq[A] = {
      xs.promise tryCompleteWith f.map(Some(_))
      xs.head onSuccess {
        case Some(result) =>
          fetch(result) match {
            case Some(nextResult) => loop(xs.tail, nextResult)
            case None             => xs.tail.promise success None
          }
      }
      xs
    }
    loop(new AsyncSeq[A], head)
  }

  def fromFuture[A](f: Future[AsyncSeq[A]])(implicit ec: EC): AsyncSeq[A] = {
    def loop(xs: AsyncSeq[A], fut: Future[AsyncSeq[A]]): AsyncSeq[A] = {
      xs.promise tryCompleteWith fut.flatMap(_.head)
      xs.head onSuccess {
        case Some(_) => loop(xs.tail, fut.map(_.tail))
      }
      xs
    }
    loop(new AsyncSeq[A], f)
  }
}
