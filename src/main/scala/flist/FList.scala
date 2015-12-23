package flist

import scala.annotation.tailrec
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.{ CanBuildFrom => CBF }
import scala.collection.mutable
import scala.concurrent.{ ExecutionContext => EC, Future }
import scala.util.{ Failure, Success }
import FList.{ single, empty }

/* A singly linked list of future values of type `A`. Its methods, such as `map` and `flatMap` will ensure that
 * the computations are executed as soon as possible, asynchronously. */
final case class FList[+A](value: FutureOption[(A, FList[A])]) {
  // Addition
  def ++[A1 >: A](that: FList[A1])(implicit ec: EC): FList[A1] = {
    def loop(head: A1, tail: FList[A1], finalTail: FList[A1]): FutureOption[(A1, FList[A1])] =
      tail.value subcoflatMap {
        case None         => (head, finalTail)
        case Some((h, t)) => (head, FList(loop(h, t, finalTail)))
      }
    FList(
      this.value flatTransform {
        case None         => that.value
        case Some((h, t)) => loop(h, t, that)
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
  def apply[A](xs: A*): FList[A] = xs.foldRight(empty[A])(_ :: _)

  def empty[A]: FList[A] = FList(FutureOption(Future successful None))

  def single[A](x: A): FList[A] = x :: empty

  def iterate[A](head: Future[Option[A]])(fetch: A => Future[Option[A]])(implicit ec: EC): FList[A] =
    FList(FutureOption(head) map (a => (a, iterate(fetch(a))(fetch))))
}
