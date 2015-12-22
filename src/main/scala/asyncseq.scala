package asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext => EC, Future }
import scala.util.{ Failure, Success }

final case class FutureOption[+A](value: Future[Option[A]]) extends AnyVal {
  // Functor-based
  def            map[B](f:        A  =>               B  )(implicit ec: EC): FutureOption[B] = FutureOption(value map (_ map f))
  def     subflatMap[B](f:        A  =>        Option[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value map (_ flatMap f))
  def      transform[B](f: Option[A] =>        Option[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value map f)
  def   subcoflatMap[B](f: Option[A] =>               B  )(implicit ec: EC): FutureOption[B] = FutureOption(value map (o => Some(f(o))))

  // Monad-based
  def        flatMap[B](f:        A  =>  FutureOption[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap { case None => Future successful None ; case Some(a) => f(a).value })
  def  flatTransform[B](f: Option[A] =>  FutureOption[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap (o => f(o).value))

  // Monad-based, accepting the underlying Repr
  def       flatMapF[B](f:        A  => Future[Option[B]])(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap { case None => Future successful None ; case Some(a) => f(a) })
  def flatTransformF[B](f: Option[A] => Future[Option[B]])(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap f)
}

// 1. singly linked list
// 2. value is a future value
// 3. once computed it could be empty
// 4. but when present it's: a value and optionally a next page value
// 5. the next page is the next cell
// list termination is either:
// * locally at #4
// * or remotely at #3

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
