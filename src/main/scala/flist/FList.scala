package flist

import flist.FList.{ empty, single }

import scala.annotation.tailrec
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.{ CanBuildFrom => CBF }
import scala.collection.mutable
import scala.concurrent.{ ExecutionContext => EC, Future }
import scala.util.{ Failure, Success }

/* A singly linked list of future values of type `A`. Its methods, such as `map` and `flatMap` will ensure that
 * the computations are executed as soon as possible, asynchronously. */
final case class FList[+A](value: FutureOption[(A, FList[A])]) {
  // Foreach
  def foreach[U](f: A => U)(implicit ec: EC): Future[Unit] =
    this.value.value flatMap {
      case None            => Future.successful(())
      case Some((h, tail)) => f(h); tail foreach f
    }

  // Size info
  def isTraversableAgain: Boolean = true

  @tailrec def hasDefiniteSize: Boolean =
    this.value.value.value match {
      case Some(Success(Some((h, tail)))) => tail.hasDefiniteSize
      case Some(Success(None))            => true
      case Some(Failure(_))               => true
      case None                           => false
    }

  def isEmpty (implicit ec: EC): Future[Boolean] = this.value.value map (_.isEmpty)
  def nonEmpty(implicit ec: EC): Future[Boolean] = this.value.value map (_.isDefined)
  def size    (implicit ec: EC): Future[Int] = this.foldLeft(0)((c, _) => c + 1)
  def length  (implicit ec: EC): Future[Int] = this.foldLeft(0)((c, _) => c + 1)

  // Addition
  def ++[A1 >: A](that: FList[A1])(implicit ec: EC): FList[A1] = {
    def loop(h0: A1, tail0: FList[A1], that: FList[A1]): FutureOption[(A1, FList[A1])] =
      tail0.value subcoflatMap {
        case None            => (h0, that)
        case Some((h, tail)) => (h0, FList(loop(h, tail, that)))
      }
    FList(
      this.value flatTransform {
        case None            => that.value
        case Some((h, tail)) => loop(h, tail, that)
      }
    )
  }

  def ++:[A1 >: A](that: FList[A1])(implicit ec: EC): FList[A1] = that ++ this
  def :::[A1 >: A](that: FList[A1])(implicit ec: EC): FList[A1] = that ++ this

  def ::[A1 >: A](x: A1): FList[A1] = FList(FutureOption(Future successful Some((x, this))))
  def +:[A1 >: A](x: A1): FList[A1] = x :: this

  def :+[A1 >: A](x: A1)(implicit ec: EC): FList[A1] = this ++ single(x)

  // Maps
  def map[B](f: A => B)(implicit ec: EC): FList[B] = FList(this.value map { case (h, t) => (f(h), t map f) })

  def flatMap[B](f: A => FList[B])(implicit ec: EC): FList[B] = this.map(f).flatten

  // Subcollections
  def tail(implicit ec: EC): Future[Option[FList[A]]] = this.value.map(_._2).value

  def drop(n: Int)(implicit ec: EC): FList[A] =
    if (n > 0) FList(this.value flatMap (_._2.value)) drop (n - 1) else this

  // Other iterators
  def grouped(size: Int)(implicit ec: EC): FList[FList[A]] = {
    require(size > 0, "size must be positive")
    def loop(xs: FList[A], n: Int): Future[(FList[A], FList[A])] = {
      if (n > 0)
        xs.value.value flatMap {
          case None            => Future successful ((empty, empty))
          case Some((h, tail)) => loop(tail, n - 1) map { case (tail2, leftover) => (h :: tail2, leftover) }
        }
      else
        Future successful ((empty, xs))
    }
    def outerLoop(xs: FList[A]): Future[FList[FList[A]]] = {
      loop(xs, size) flatMap { case (inner, leftover) =>
        leftover.isEmpty flatMap {
          case true  => Future successful (inner :: empty)
          case false => outerLoop(leftover) map (inner :: _)
        }
      }
    }
    FList fromFuture outerLoop(this)
  }

  // Folds
  def foldLeft[B](z: B)(op: (B, A) => B)(implicit ec: EC): Future[B] =
    this.value.value.flatMap {
      case None         => Future successful z
      case Some((h, t)) => t.foldLeft(op(z, h))(op)
    }

  // Specific Folds
  def flatten[B](implicit ec: EC, ev: A <:< FList[B]): FList[B] =
    FList(this.map(ev).value flatMap { case (h, t) => (h ++ t.flatten).value })

  // Conversions
  private def fromBuilder[A1 >: A, CC[_]](b: mutable.Builder[A1, CC[A1]])(implicit ec: EC): Future[CC[A1]] =
    foldLeft(b)(_ += _).map(_.result)

  def to[Col[_]](implicit cbf: CBF[Nothing, A, Col[A @uV]], ec: EC): Future[Col[A @uV]] = fromBuilder(cbf())

  def toVector(implicit ec: EC): Future[Vector[A]] = to[Vector]

  def toMap[K, V](implicit ec: EC, ev: A <:< (K, V)): Future[Map[K, V]] =
    foldLeft(Map.newBuilder[K, V])(_ += _) map (_.result)

  // Strings
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    @tailrec def loop(xs: FList[A], first: Boolean): Unit = {
      xs.value.value.value match {
        case None                        => if (!first) b append sep; b append '?'
        case Some(Failure(e))            => if (!first) b append sep; b append s"[ex: $e]"
        case Some(Success(None))         =>
        case Some(Success(Some((h, t)))) => if (!first) b append sep; b append h; loop(t, first = false)
      }
    }

    b append start
    loop(this, first = true)
    b append end
    b
  }

  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  override def toString = this.mkString("FList(", ", ", ")")
}

object FList {
  def empty[A]: FList[A] = FList(FutureOption(Future successful None))

  def single[A](x: A): FList[A] = x :: empty

  def apply[A](xs: A*): FList[A]       = xs.foldRight(empty[A])(_ :: _)
  def fromSeq[A](xs: Seq[A]): FList[A] = xs.foldRight(empty[A])(_ :: _)

  def iterate[A](head: Future[Option[A]])(fetch: A => Future[Option[A]])(implicit ec: EC): FList[A] =
    FList(FutureOption(head) map (a => (a, iterate(fetch(a))(fetch))))

  def unpaginate[A](head: Future[A])(fetch: A => Option[Future[A]])(implicit ec: EC): FList[A] = {
    def loop(head: Future[Option[A]]): FList[A] = FList(FutureOption(head) map (a => (a, loop(fetch2(a)))))
    def fetch2(a: A) = fetch(a).fold(Future successful Option.empty[A])(_ map (Some(_)))
    loop(head map (Some(_)))
  }

  def fromFuture[A](f: Future[FList[A]])(implicit ec: EC): FList[A] =
    FList(FutureOption(f flatMap (_.value.value)))
}
