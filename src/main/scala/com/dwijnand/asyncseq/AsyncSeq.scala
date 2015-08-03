package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.{ CanBuildFrom => CBF }
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.reflect.ClassTag
import scala.util.{ Failure, Success }
import scala.{ PartialFunction => ?=> }

final class AsyncSeq[+A] private {
  private[AsyncSeq] val promise: Promise[Option[A @uV]] = Promise[Option[A]]()

  val head: Future[Option[A]] = promise.future

  lazy val tail: AsyncSeq[A] = new AsyncSeq[A]

  // Size info
  @tailrec def hasDefiniteSize: Boolean =
    head.value match {
      case None                => false
      case Some(Success(None)) => true
      case Some(Success(_))    => tail.hasDefiniteSize
      case Some(Failure(_))    => true
    }

  def isTraversableAgain: Boolean = true

  def isEmpty (implicit ec: EC): Future[Boolean] = head.map(_.isEmpty)
  def nonEmpty(implicit ec: EC): Future[Boolean] = head.map(_.nonEmpty)
  def size    (implicit ec: EC): Future[Int]     = foldLeft(0)((c, _) => c + 1)
  def length  (implicit ec: EC): Future[Int]     = foldLeft(0)((c, _) => c + 1)

  def foreach[U](f: A => U)(implicit ec: EC): Future[Unit] =
    head flatMap {
      case None    => Future.successful(())
      case Some(x) => f(x) ; tail foreach f
    }

  // Iterators
  def iterator(implicit ec: EC): Future[Iterator[A]] = toIterator

  // Element Retrieval
//def head: Future[Option[A]] = head

  def last(implicit ec: EC): Future[Option[A]] = {
    def loop(xs: AsyncSeq[A]): Future[Option[A]] =
      xs.tail.head flatMap {
        case None    => xs.head
        case Some(_) => loop(xs.tail)
      }
    head flatMap {
      case None    => head
      case Some(x) => loop(this)
    }
  }

  def find(p: A => Boolean)(implicit ec: EC): Future[Option[A]] =
    head flatMap {
      case Some(x) if p(x) => Future successful Some(x)
      case Some(_)         => tail find p
      case None            => Future successful None
    }

  // Indexing and Length
  // TODO: Future[Option[A]] ?
  def apply(idx: Int)(implicit ec: EC): Future[A] = {
    def ex = new scala.IndexOutOfBoundsException("" + idx)
    if (idx < 0) Future failed ex
    else drop(idx).head.map(_.getOrElse(throw ex))
  }

  def isDefinedAt(idx: Int)(implicit ec: EC): Future[Boolean] =
    if (idx < 0) Future successful false
    else lengthCompare(idx).map(_ > 0)

  def lengthCompare(len: Int)(implicit ec: EC): Future[Int] = {
    def loop(i: Int, xs: AsyncSeq[A]): Future[Int] =
      xs.isEmpty flatMap {
        case true if i == len => Future successful 0
        case true if i < len  => Future successful -1
        case true if i > len  => Future successful 1
        case false            => loop(i + 1, xs.tail)
      }
    if (len < 0) Future successful 1
    else loop(0, this)
  }

  def indices(implicit ec: EC): AsyncSeq[Int] = ??? /// 0 until length

  // Index Search
  def indexOf[A1 >: A](x: A1)                            : Future[Int] = indexWhere(_ == x)
  def indexOf[A1 >: A](x: A1, from: Int)                 : Future[Int] = ???
  def lastIndexOf[A1 >: A](x: A1)                        : Future[Int] = ???
  def lastIndexOf[A1 >: A](x: A1, end: Int)              : Future[Int] = ???
  def indexWhere(p: A => Boolean, from: Int)             : Future[Int] = ???
  def indexWhere(p: A => Boolean)                        : Future[Int] = ???
  def lastIndexWhere[A1 >: A](x: A1)                     : Future[Int] = ???
  def lastIndexWhere[A1 >: A](x: A1, end: Int)           : Future[Int] = ???
  def segmentLength(p: A => Boolean, from: Int)          : Future[Int] = ???
  def prefixLength(p: A => Boolean)                      : Future[Int] = ???
  def indexOfSlice[A1 >: A](ys: AsyncSeq[A1])            : Future[Int] = ???
  def indexOfSlice[A1 >: A](ys: AsyncSeq[A1], from: Int) : Future[Int] = ???

  // Addition
  def ++[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): AsyncSeq[A1] = {
    def loopThis(xs: AsyncSeq[A1], xs0: AsyncSeq[A]): AsyncSeq[A1] = {
      xs0.head onComplete {
        case Success(Some(x)) => xs.promise success Some(x) ; loopThis(xs.tail, xs0.tail)
        case Success(None)    => loopThat(xs, that)
        case f                => xs.promise tryComplete f
      }
      xs
    }
    def loopThat(xs: AsyncSeq[A1], xs0: AsyncSeq[A1]): Unit = {
      xs0.head onComplete {
        case Success(Some(x)) => xs.promise success Some(x) ; loopThat(xs.tail, xs0.tail)
        case Success(None)    => xs.promise success None
        case f                => xs.promise tryComplete f
      }
    }
    loopThis(new AsyncSeq[A1], this)
  }

  def ++:[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): AsyncSeq[A1] = {
    def loopThis(xs: AsyncSeq[A1], xs0: AsyncSeq[A]): Unit = {
      xs0.head onComplete {
        case Success(Some(x)) => xs.promise success Some(x) ; loopThis(xs.tail, xs0.tail)
        case Success(None)    => xs.promise success None
        case f                => xs.promise tryComplete f
      }
    }
    def loopThat(xs: AsyncSeq[A1], xs0: AsyncSeq[A1]): AsyncSeq[A1] = {
      xs0.head onComplete {
        case Success(Some(x)) => xs.promise success Some(x) ; loopThat(xs.tail, xs0.tail)
        case Success(None)    => loopThis(xs, this)
        case f                => xs.promise tryComplete f
      }
      xs
    }
    loopThat(new AsyncSeq[A1], that)
  }

  // TODO: Hmm if AsyncSeq were a sealed trait, this would just be a cons with a cast.
  def +:[A1 >: A](x: A1)(implicit ec: EC): AsyncSeq[A1] = {
    def loop(x: A1, xs: AsyncSeq[A1], xs0: AsyncSeq[A]): AsyncSeq[A1] = {
      xs.promise success Some(x)
      xs0.head onComplete {
        case Success(Some(x)) => loop(x, xs.tail, xs0.tail)
        case f                => xs.tail.promise complete f
      }
      xs
    }
    loop(x, new AsyncSeq[A1], this)
  }

  def :+[A1 >: A](x: A1)(implicit ec: EC): AsyncSeq[A1] = {
    def loop(xs: AsyncSeq[A1], xs0: AsyncSeq[A]): AsyncSeq[A1] = {
      xs0.head onComplete {
        case Success(Some(x)) => xs.promise success Some(x) ; loop(xs.tail, xs0.tail)
        case Success(None)    => xs.promise success Some(x) ; xs.tail.promise success None
        case f                => xs.promise complete f
      }
      xs
    }
    loop(new AsyncSeq[A1], this)
  }

  def padTo[A1 >: A](len: Int, x: A1)  : AsyncSeq[A1] = ???

  // Updates
  def patch[A1 >: A](from: Int, patch: AsyncSeq[A1], replaced: Int): AsyncSeq[A1] = ???
  def updated[A1 >: A](idx: Int, x: A1): AsyncSeq[A1] = ???

  // Sorting
  def sorted[A1 >: A](implicit ord: Ordering[A1])     : AsyncSeq[A] = ???
  def sortWith(lt: (A, A) => Boolean)                 : AsyncSeq[A] = ???
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]) : AsyncSeq[A] = sorted(ord on f)

  // Reversals
  def reverse: AsyncSeq[A]                  = ???
  def reverseIterator: Future[Iterator[A]]  = ???
  def reverseMap[B](f: A => B): AsyncSeq[B] = ???

  // Multiset Operations
  def union[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): AsyncSeq[A1] = this ++ that

  def intersect[A1 >: A](that: AsyncSeq[A1]): AsyncSeq[A] = ???
  def diff     [A1 >: A](that: AsyncSeq[A1]): AsyncSeq[A] = ???
  def distinct                              : AsyncSeq[A] = ???

  // Comparisons
  def sameElements[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): Future[Boolean] = {
    val xy = for { x <- this.head ; y <- that.head } yield (x, y)
    xy flatMap {
      case (Some(x), Some(y)) if x == y => this.tail sameElements that.tail
      case (None, None)                 => Future successful true
      case _                            => Future successful false
    }
  }

  def startsWith[B](that: AsyncSeq[B])                        : Future[Boolean] = ???
  def startsWith[B](that: AsyncSeq[B], offset: Int)           : Future[Boolean] = ???
  def endsWith[B](that: AsyncSeq[B])                          : Future[Boolean] = ???
  def contains[A1 >: A](x: A1)(implicit ec: EC)               : Future[Boolean] = exists(_ == x)
  def containsSlice[B](that: AsyncSeq[B])                     : Future[Boolean] = ???
  def corresponds[B](that: AsyncSeq[B])(p: (A, B) => Boolean) : Future[Boolean] = ???

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
  def collect[B](pf: A ?=> B)                          : AsyncSeq[B] = ???

  // Subcollections
//def tail                         : AsyncSeq[A] = tail
  def init                         : AsyncSeq[A] = dropRight(1)
  def slice(from: Int, until: Int) : AsyncSeq[A] = ???

  def drop(n: Int)(implicit ec: EC): AsyncSeq[A] = {
    if (n <= 0) this
    else AsyncSeq fromFuture isEmpty.map(if (_) this else tail drop n - 1)
  }

  def dropRight(n: Int): AsyncSeq[A] = ???

  def dropWhile(p: A => Boolean)(implicit ec: EC): AsyncSeq[A] = {
    AsyncSeq fromFuture
      head.map {
        case Some(x) if p(x) => tail dropWhile p
        case Some(_)         => tail
        case None            => this
      }
  }

  def take(n: Int)                 : AsyncSeq[A] = ???
  def takeRight(n: Int)            : AsyncSeq[A] = ???
  def takeWhile(p: A => Boolean)   : AsyncSeq[A] = ???
  def filter(   p: A => Boolean)   : AsyncSeq[A] = filterImpl(p, isFlipped = false)
  def filterNot(p: A => Boolean)   : AsyncSeq[A] = filterImpl(p, isFlipped = true)

  private def filterImpl(p: A => Boolean, isFlipped: Boolean): AsyncSeq[A] = ???

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
          case Success(Some(x)) => wip.promise success Some(x) ; loop(xs0.tail, xs, wip.tail, n - 1, grouped)
          case t                => wip.promise complete t ; grouped.promise success Some(xs)
        }
      } else {
        wip.promise success None
        grouped.promise success Some(xs)
        loop0(xs0, grouped.tail)
      }
      grouped
    }
    loop0(this, new AsyncSeq[AsyncSeq[A]])
  }

  def sliding(size: Int)            : AsyncSeq[Vector[A]] = ???
  def sliding(size: Int, step: Int) : AsyncSeq[Vector[A]] = ???

  // Zippers
  def zip[A1 >: A, B](ys: AsyncSeq[B])                 : AsyncSeq[(A1, B)]   = ???
  def zipAll[B, A1 >: A](ys: AsyncSeq[B], a: A1, b: B) : AsyncSeq[(A1, B)]   = ???
  def zipWithIndex[A1 >: A]                            : AsyncSeq[(A1, Int)] = ???

  // Subdivisions
  def splitAt(n: Int)            : (AsyncSeq[A], AsyncSeq[A])  = ???
  def span(p: A => Boolean)      : (AsyncSeq[A], AsyncSeq[A])  = ???
  def partition(p: A => Boolean) : (AsyncSeq[A], AsyncSeq[A])  = ???
  def groupBy[K](f: A => K)      : Future[Map[K, AsyncSeq[A]]] = ???

  // Element Conditions
  def forall(p: A => Boolean)(implicit ec: EC): Future[Boolean] =
    head flatMap {
      case Some(x) if p(x) => tail forall p
      case Some(_)         => Future successful false
      case None            => Future successful true
    }

  def exists(p: A => Boolean)(implicit ec: EC): Future[Boolean] =
    head flatMap {
      case Some(x) if p(x) => Future successful true
      case Some(_)         => tail exists p
      case None            => Future successful false
    }

  def count(p: A => Boolean)(implicit ec: EC): Future[Int] =
    foldLeft(0)((res, x) => if (p(x)) res + 1 else res)

  // Folds
  def fold  [A1 >: A](z: A1)(op: (A1, A1) => A1)(implicit ec: EC): Future[A1]         = foldLeft(z)(op)
  def reduce[A1 >: A]       (op: (A1, A1) => A1)(implicit ec: EC): Future[Option[A1]] = reduceLeft(op)

  def foldLeft[B](z: B)(op: (B, A) => B)(implicit ec: EC): Future[B] = {
    head flatMap {
      case Some(x) => tail.foldLeft(op(z, x))(op)
      case None    => Future successful z
    }
  }

  def foldRight[B](z: B)(op: (A, B) => B)(implicit ec: EC): Future[B] = {
    head flatMap {
      case Some(x) => tail.foldRight(z)(op).map(b => op(x, b))
      case None    => Future successful z
    }
  }

  def reduceLeft[A1 >: A](op: (A1, A) => A1)(implicit ec: EC): Future[Option[A1]] =
    head flatMap {
      case Some(x) => tail.foldLeft[A1](x)(op).map(Some(_))
      case None    => head
    }

  def reduceRight[A1 >: A](op: (A, A1) => A1)(implicit ec: EC): Future[Option[A1]] = {
    def loop(x: A, xs: AsyncSeq[A]): Future[Option[A1]] =
      xs.tail.head flatMap {
        case Some(y) => loop(y, xs.tail).map(b => b.map(b => op(x, b)))
        case None    => xs.head
      }
    head flatMap {
      case Some(x) => loop(x, this)
      case None    => head
    }
  }

  // Specific Folds
  def sum    [A1 >: A](implicit num: Numeric[A1], ec: EC): Future[A1] = foldLeft(num.zero)(num.plus)
  def product[A1 >: A](implicit num: Numeric[A1], ec: EC): Future[A1] = foldLeft(num.one)(num.times)

  def min[A1 >: A](implicit ord: Ordering[A1], ec: EC): Future[Option[A]] =
    reduceLeft((x, y) => if (ord.lteq(x, y)) x else y)

  def max[A1 >: A](implicit ord: Ordering[A1], ec: EC): Future[Option[A]] =
    reduceLeft((x, y) => if (ord.gteq(x, y)) x else y)

  private def reduceBy[B](f: A => B)(p: (B, B) => Boolean)(implicit ec: EC): Future[Option[A]] = {
    head flatMap {
      case Some(x) =>
        foldLeft((x, f(x))) { case ((x, fx), y) =>
          val fy = f(y)
          if (p(fx, fy)) (x, fx) else (y, fy)
        }
        .map(t => Some(t._1))
      case None    => head
    }
  }

  def minBy[B](f: A => B)(implicit ord: Ordering[B], ec: EC): Future[Option[A]] = reduceBy(f)(ord.lt)
  def maxBy[B](f: A => B)(implicit ord: Ordering[B], ec: EC): Future[Option[A]] = reduceBy(f)(ord.gt)

  def scan[B >: A](z: B)(op: (B, B) => B): AsyncSeq[B] = ???
  def scanLeft[B]( z: B)(op: (B, A) => B): AsyncSeq[B] = ???
  def scanRight[B](z: B)(op: (A, B) => B): AsyncSeq[B] = ???

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

  // Copying
  def copyToBuffer[B >: A](xs: mutable.Buffer[B])(implicit ec: EC): Future[Unit] = toVector.map(xs ++= _)

  def copyToArray[B >: A](arr: Array[B])                      (implicit ec: EC): Future[Unit] = copyToArray(arr, 0, arr.length)
  def copyToArray[B >: A](arr: Array[B], start: Int)          (implicit ec: EC): Future[Unit] = copyToArray(arr, start, arr.length - start)
  def copyToArray[B >: A](arr: Array[B], start: Int, len: Int)(implicit ec: EC): Future[Unit] = {
    val end = (start + len) min arr.length
    def loop(xs: AsyncSeq[A], i: Int): Future[Unit] = {
      xs.head flatMap {
        case Some(x) if i < end => arr(i) = x ; loop(xs.tail, i + 1)
        case _                  => Future.successful(())
      }
    }
    loop(this, start)
  }

  // Conversions
  private def fromBuilder[A1 >: A, CC[_]](b: mutable.Builder[A1, CC[A1]])(implicit ec: EC): Future[CC[A1]] =
    foldLeft(b)(_ += _).map(_.result)

  def to[Col[_]](implicit cbf: CBF[Nothing, A, Col[A @uV]], ec: EC): Future[Col[A @uV]] = fromBuilder(cbf())

  private def to2[A1 >: A, Col[_]](implicit cbf: CBF[Nothing, A1, Col[A1 @uV]], ec: EC): Future[Col[A1 @uV]] =
    fromBuilder(cbf())

  def toArray [A1 >: A: ClassTag](implicit ec: EC): Future[Array[A1]]          = to2[A1, Array]
  def toBuffer[A1 >: A]          (implicit ec: EC): Future[mutable.Buffer[A1]] = to2[A1, mutable.Buffer]
  def toSet   [A1 >: A]          (implicit ec: EC): Future[Set[A1]]            = to2[A1, Set]

  def toList  (implicit ec: EC): Future[List[A]]   = to[List]
  def toStream(implicit ec: EC): Future[Stream[A]] = to[Stream]
  def toVector(implicit ec: EC): Future[Vector[A]] = to[Vector]

  def toMap[K, V](implicit ec: EC, ev: A <:< (K, V)): Future[Map[K, V]] =
    foldLeft(Map.newBuilder[K, V])(_ += _).map(_.result)

  def toIterator   (implicit ec: EC): Future[Iterator[A]]    = toVector.map(_.iterator)
  def toIndexedSeq (implicit ec: EC): Future[IndexedSeq[A]]  = toVector
  def toSeq        (implicit ec: EC): Future[Seq[A]]         = toIndexedSeq
  def toIterable   (implicit ec: EC): Future[Iterable[A]]    = toSeq
  def toTraversable(implicit ec: EC): Future[Traversable[A]] = toIterable

  def toStreamFuture: Stream[Future[Option[A]]] = {
    lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(this, stream.map(_.tail))
    stream.map(_.head)
  }

  // Strings
  def mkString             : String = mkString("")
  def mkString(sep: String): String = mkString("", sep, "")

  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  def addString(b: StringBuilder)             : StringBuilder = addString(b, "")
  def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    @tailrec def loop(xs: AsyncSeq[A], first: Boolean): Unit = {
      xs.head.value match {
        case None                   => if (!first) b append sep; b append '?'
        case Some(Success(None))    =>
        case Some(Success(Some(x))) => if (!first) b append sep; b append x ; loop(xs.tail, first = false)
        case Some(Failure(t))       => if (!first) b append sep; b append s"[ex: $t]"
      }
    }

    b append start
    loop(this, first = true)
    b append end
    b
  }

  override def toString = this.mkString("AsyncSeq(", ", ", ")")
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
        case Some(_) => loop(xs.tail, f.map(_.tail))
      }
      xs
    }
    loop(new AsyncSeq[A], f)
  }
}
