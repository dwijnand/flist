package asyncseq

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext => EC, Future }
import scala.util.{ Failure, Success }

// TODO: AnyVal, toString, final, sealed

// 1. singly linked list
// 2. value is a future value
// 3. once computed it could be empty
// 4. but when present it's: a value and optionally a next page value
// 5. the next page is the next cell
// list termination is either:
// * locally at #4
// * or remotely at #3

final case class FList[+A](value: Future[Option[(A, Option[FList[A]])]]) {
  def map[B](f: A => B)(implicit ec: EC): FList[B] = {
    FList(value flatMap {
      case None                     => Future successful None
      case Some((head, None))       => Future successful Some((f(head), None))
      case Some((head, Some(tail))) => Future successful Some((f(head), Some(tail map f)))
    })
  }

  /*
  flatten (us1 :: us2 :: FNil) ::
          (uk1 :: uk2 :: FNil) ::
          (au1 :: au2 :: FNil) ::
          FNil
       to us1 :: us2 :: uk1 :: uk2 :: au1 :: au2 :: FNil
   */
  def flatten[B](implicit ec: EC, ev: A <:< FList[B]): FList[B] = {
    def inner(head: B, optTail: Option[FList[B]], finalTail: Option[FList[B]]): Future[Option[(B, Option[FList[B]])]] = {
      optTail match {
        case None       => Future successful Some((head, finalTail))
        case Some(tail) =>
          tail.value flatMap {
            case None            => Future successful Some((head, finalTail))
            case Some((h, optT)) => Future successful Some((head, Some(FList(inner(h, optT, finalTail)))))
          }
      }
    }
    def loop(target: FList[FList[B]]): Future[Option[(B, Option[FList[B]])]] = {
      target.value flatMap {
        case None                      => Future successful None
        case Some((xsHead, xsOptTail)) =>
          xsHead.value flatMap {
            case None               =>
              xsOptTail match {
                case None         => Future successful None
                case Some(xsTail) => loop(xsTail)
              }
            case Some((head, optTail)) => inner(head, optTail, xsOptTail map (_.flatten))
          }
      }
    }
    FList(loop(this map ev))
  }

  // Strings
  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    @tailrec def loop(xs: FList[A], first: Boolean): Unit = {
      xs.value.value match {
        case None                              => if (!first) b append sep; b append '?'
        case Some(Failure(e))                  => if (!first) b append sep; b append s"[ex: $e]"
        case Some(Success(None))               =>
        case Some(Success(Some((h, None))))    => if (!first) b append sep; b append h;
        case Some(Success(Some((h, Some(t))))) => if (!first) b append sep; b append h; loop(t, first = false)
      }
    }

    b append start
    loop(this, first = true)
    b append end
    b
  }

  override def toString = this.mkString("FList(", ", ", ")")
}

sealed trait FutureList[+A] extends Product with Serializable {
  def headOption(implicit ec: EC): Future[Option[A]] =
    this match {
      case FutureNil         => Future successful None
      case FutureCons(h, _) => h map (Some(_))
    }

  def tailOption(implicit ec: EC): Future[Option[FutureList[A]]] =
    this match {
      case FutureNil        => Future successful Some(FutureNil)
      case FutureCons(_, t) => t map (Some(_))
    }

  def map[B](f: A => B)(implicit ec: EC): FutureList[B] =
    this match {
      case FutureNil        => FutureNil
      case FutureCons(h, t) => FutureCons(h map f, t map (_ map f))
    }

  def flatMap[B](f: A => FutureList[B])(implicit ec: EC): FutureList[B] =
    this match {
      case FutureNil        => FutureNil
      case FutureCons(h, t) => map(f).flatten
    }

  def flatten[B](implicit ec: EC, ev: A <:< FutureList[B]): FutureList[B] = {
    val target: FutureList[FutureList[B]] = this map ev
    target match {
      case FutureNil                  => FutureNil
      case FutureCons(xsHead, xsTail) =>
//        valueList flatMap {
//          case (FutureNil, tailList)         => tailList.flatten
//          case (FutureCons(value), tailList) =>
//            value map { case (head, tail) =>
//              FutureCons(tailList.flatten map (tail2 => (head, tail ++ tail2)))
//            }
//        }
        ??? : FutureList[B]
    }
  }

  def ++[A1 >: A](that: FutureList[A1])(implicit/**/ ec: EC): FutureList[A1] =
    this match {
      case FutureNil        => that
      case FutureCons(h, t) => FutureCons(h, t map (_ ++ that))
    }
}

final case class FutureCons[A](head: Future[A], tail: Future[FutureList[A]]) extends FutureList[A]

sealed trait FutureNil extends FutureList[Nothing] {
  override def toString = "FutureNil"
}

final case object FutureNil extends FutureNil

//final case class FutureList[A](value: Future[Page[A]]) {
//  def head(implicit ec: EC): Future[A] = value map (_.head)
//
//  def tail(implicit ec: EC): Future[Option[FutureList[A]]] = value map (p => p.tail map (FutureList(_)))
//
//  def map[B](f: A => B)(implicit ec: EC): FutureList[B] = FutureList(value map (_ map f))
//
//  def flatMap[B](f: A => FutureList[B])(implicit ec: EC): FutureList[B] = {
//    FutureList(
//      value flatMap {
//        case ConsPage(a, ft) => f(a).value map (_ concat ft.map(_ map f))
//        case LastPage(a)     => f(a).value
//      }
//    )
//    ???
//  }
//
//  def concat(rhs: Future[Page[A]])(implicit ec: EC): Page[A] =
//    ???
//}

//final case class AsyncSeq[A](value: Future[(A, Option[AsyncSeq[A]])]) {
//  def head(implicit ec: EC): Future[A]                   = value map (_._1)
//  def tail(implicit ec: EC): Future[Option[AsyncSeq[A]]] = value map (_._2)
//
//  def map[B](f: A => B)(implicit ec: EC): AsyncSeq[B] =
//    AsyncSeq(value map { case (head, tail) => (f(head), tail map (xs => xs map f)) })
//
//  def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC): AsyncSeq[B] = this.map(f).flatten
//
//  def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B] = {
//    val xs: AsyncSeq[AsyncSeq[B]] = this map ev
//    AsyncSeq {
//      val origValue: Future[(AsyncSeq[B], Option[AsyncSeq[AsyncSeq[B]]])] = xs.value
//      origValue flatMap { case (origHead: AsyncSeq[B], origTail: Option[AsyncSeq[AsyncSeq[B]]]) =>
//        val origHeadValue: Future[(B, Option[AsyncSeq[B]])] = origHead.value
//        origHeadValue map {
//          case (b: B, None) => (b, origTail)
//          case (b: B, Some(tail2: AsyncSeq[B])) =>
//            (b,
//              tail2
//              )
//        }
//        ??? : Future[(B, Option[AsyncSeq[B]])]
//      }
//      ??? : Future[(B, Option[AsyncSeq[B]])]
//    }
//  }
//
//  //  def concat(rhs: Future[AsyncSeq[A]])(implicit ec: EC): AsyncSeq[A] =
////    this match {
////      case Last(a)     => Cons(a, rhs)
////      case Cons(a, ft) => Cons(a, ft map (_ concat rhs))
////    }
//}
//
//object AsyncSeq {
//  def unpag[A](head: Future[A], call: A => Option[Future[A]])(implicit ec: EC): AsyncSeq[A] =
//    AsyncSeq(head map (a => (a, call(a) map (fa => unpag(fa, call)))))
//}
