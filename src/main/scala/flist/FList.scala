package flist

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext => EC }
import scala.util.{ Failure, Success }

/* A singly linked list of future values of type `A`. Its methods, such as `map` and `flatMap` will ensure that
 * the computations are executed as soon as possible, asynchronously. */
final case class FList[+A](value: FutureOption[(A, FList[A])]) {
  def map[B](f: A => B)(implicit ec: EC): FList[B] = FList(this.value map { case (h, t) => (f(h), t map f) })

  def flatMap[B](f: A => FList[B])(implicit ec: EC): FList[B] = this.map(f).flatten

  def flatten[B](implicit ec: EC, ev: A <:< FList[B]): FList[B] =
    FList(this.map(ev).value flatMap { case (h, t) => (h ++ t.flatten).value })

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
