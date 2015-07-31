package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.reflect.ClassTag
import scala.util.{ Failure, Success }
import scala.{ PartialFunction => ?=> }

sealed trait AsyncSeq[+A] extends Any {
  def head: Future[Option[A]]
  def tail: AsyncSeq[A]
}

object AsyncSeq {
  // TODO: Consider an Ops that captures the EC at the top
  implicit final class AsyncSeqOps[A](private val xs: AsyncSeq[A]) extends AnyVal {
    def foreach[U](f: A => U)(implicit ec: EC): Future[Unit] =
      xs.head flatMap {
        case None    => Future.successful(())
        case Some(x) => f(x) ; xs.tail foreach f
      }

    // Size info

    @tailrec def hasDefiniteSize: Boolean =
      xs.head.value match {
        case None                => false
        case Some(Success(None)) => true
        case Some(Success(_))    => xs.tail.hasDefiniteSize
        case Some(Failure(_))    => true
      }

    def size(implicit ec: EC)     : Future[Int]     = count(_ => true)
    def isEmpty(implicit ec: EC)  : Future[Boolean] = xs.head.map(_.isEmpty)
    def nonEmpty(implicit ec: EC) : Future[Boolean] = xs.head.map(_.nonEmpty)

    // Iterators
    def iterator: Future[Iterator[A]] = ???

    // Element Retrieval
    def head: Future[Option[A]] = xs.head

    def last(implicit ec: EC): Future[Option[A]] =
      xs.head flatMap {
        case None    => xs.head
        case Some(x) => xs.tail.last
      }

    def find(p: A => Boolean)(implicit ec: EC): Future[Option[A]] =
      xs.head flatMap {
        case Some(x) if p(x) => Future successful Some(x)
        case Some(_)         => xs.tail find p
        case None            => Future successful None
      }

    // Indexing and Length
    def apply(idx: Int)(implicit ec: EC): Future[A] = {
      def ex = new scala.IndexOutOfBoundsException("" + idx)
      if (idx < 0) Future failed ex
      else drop(idx).head.map(_.getOrElse(throw ex))
    }

    def isDefinedAt(idx: Int)(implicit ec: EC): Future[Boolean] =
      if (idx < 0) Future successful false
      else lengthCompare(idx) map (_ > 0)

    def length(implicit ec: EC): Future[Int] = size

    def lengthCompare(len: Int)(implicit ec: EC): Future[Int] = {
      def loop(i: Int, xs: AsyncSeq[A]): Future[Int] =
        xs.isEmpty flatMap {
          case true if i == len => Future successful 0
          case true if i < len  => Future successful -1
          case true if i > len  => Future successful 1
          case false            => loop(i + 1, xs.tail)
        }
      if (len < 0) Future successful 1
      else loop(0, xs)
    }

    def indices: AsyncSeq[Int] = ???

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
    def ++[A1 >: A]( ys: AsyncSeq[A1])     : AsyncSeq[A1] = ???
    def ++:[A1 >: A](ys: AsyncSeq[A1])     : AsyncSeq[A1] = ???
    def +:[A1 >: A](elem: A1)              : AsyncSeq[A1] = ???
    def :+[A1 >: A](elem: A1)              : AsyncSeq[A1] = ???
    def padTo[A1 >: A](len: Int, elem: A1) : AsyncSeq[A1] = ???

    // Updates
    def patch[A1 >: A](from: Int, patch: AsyncSeq[A1], replaced: Int): AsyncSeq[A1] = ???
    def updated[A1 >: A](idx: Int, elem: A1): AsyncSeq[A1] = ???

    // Sorting
    def sorted[A1 >: A](implicit ord: Ordering[A1])     : AsyncSeq[A] = ???
    def sortWith(lt: (A, A) => Boolean)                 : AsyncSeq[A] = ???
    def sortBy[B](f: A => B)(implicit ord: Ordering[B]) : AsyncSeq[A] = sorted(ord on f)

    // Reversals
    def reverse: AsyncSeq[A] = ???
    def reverseIterator: Future[Iterator[A]] = ???
    def reverseMap[B](f: A => B): AsyncSeq[B] = ???

    // Multiset Operations
    def intersect[A1 >: A](ys: AsyncSeq[A1]): AsyncSeq[A] = ???
    def diff     [A1 >: A](ys: AsyncSeq[A1]): AsyncSeq[A] = ???
    def union    [A1 >: A](ys: AsyncSeq[A1]): AsyncSeq[A1] = xs ++ ys
    def distinct: AsyncSeq[A] = ???

    // Comparisons
    def sameElements[A1 >: A](ys: AsyncSeq[A1])(implicit ec: EC): Future[Boolean] = {
      val xy = for { x <- xs.head ; y <- ys.head } yield (x, y)
      xy flatMap {
        case (Some(x), Some(y)) if x == y => xs.tail sameElements ys.tail
        case (None, None)                 => Future successful true
        case _                            => Future successful false
      }
    }

    def startsWith[B](ys: AsyncSeq[B])                        : Future[Boolean] = ???
    def startsWith[B](ys: AsyncSeq[B], offset: Int)           : Future[Boolean] = ???
    def endsWith[B](ys: AsyncSeq[B])                          : Future[Boolean] = ???
    def contains[A1 >: A](x: A1)(implicit ec: EC)             : Future[Boolean] = exists(_ == x)
    def containsSlice[B](ys: AsyncSeq[B])                     : Future[Boolean] = ???
    def corresponds[B](ys: AsyncSeq[B])(p: (A, B) => Boolean) : Future[Boolean] = ???

    // Maps
    def map[B](f: A => B)(implicit ec: EC)               : AsyncSeq[B] = AsyncSeq.map(xs, f)
    def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC) : AsyncSeq[B] = AsyncSeq.flatMap(xs, f)
    def collect[B](pf: A ?=> B)                          : AsyncSeq[B] = ???

    // Subcollections
    def tail                         : AsyncSeq[A] = xs.tail
    def init                         : AsyncSeq[A] = dropRight(1)
    def slice(from: Int, until: Int) : AsyncSeq[A] = ???
    def drop(n: Int)                 : AsyncSeq[A] = ???
    def dropRight(n: Int)            : AsyncSeq[A] = ???
    def dropWhile(p: A => Boolean)   : AsyncSeq[A] = ???
    def take(n: Int)                 : AsyncSeq[A] = ???
    def takeRight(n: Int)            : AsyncSeq[A] = ???
    def takeWhile(p: A => Boolean)   : AsyncSeq[A] = ???
    def filter(   p: A => Boolean)   : AsyncSeq[A] = filterImpl(p, isFlipped = false)
    def filterNot(p: A => Boolean)   : AsyncSeq[A] = filterImpl(p, isFlipped = true)

    private def filterImpl(p: A => Boolean, isFlipped: Boolean): AsyncSeq[A] = ???

    // Other iterators
    def grouped(size: Int)            : Future[Iterator[AsyncSeq[A]]] = ???
    def sliding(size: Int)            : Future[Iterator[AsyncSeq[A]]] = ???
    def sliding(size: Int, step: Int) : Future[Iterator[AsyncSeq[A]]] = ???

    // Zippers
    def zip[A1 >: A, B](ys: AsyncSeq[B])                 : AsyncSeq[(A1, B)]   = ???
    def zipAll[B, A1 >: A](ys: AsyncSeq[B], a: A1, b: B) : AsyncSeq[(A1, B)]   = ???
    def zipWithIndex[A1 >: A]                            : AsyncSeq[(A1, Int)] = ???

    // Subdivisions
    def splitAt(n: Int)            : Future[(AsyncSeq[A], AsyncSeq[A])] = ???
    def span(p: A => Boolean)      : Future[(AsyncSeq[A], AsyncSeq[A])] = ???
    def partition(p: A => Boolean) : Future[(AsyncSeq[A], AsyncSeq[A])] = ???
    def groupBy[K](f: A => K)      : Future[Map[K, AsyncSeq[A]]]        = ???

    // Element Conditions
    def forall(p: A => Boolean)(implicit ec: EC): Future[Boolean] =
      xs.head flatMap {
        case Some(x) if p(x) => xs.tail forall p
        case Some(_)         => Future successful false
        case None            => Future successful true
      }

    def exists(p: A => Boolean)(implicit ec: EC): Future[Boolean] =
      xs.head flatMap {
        case Some(x) if p(x) => Future successful true
        case Some(_)         => xs.tail exists p
        case None            => Future successful false
      }

    def count(p: A => Boolean)(implicit ec: EC): Future[Int] =
      foldLeft(0)((res, x) => if (p(x)) res + 1 else res)

    // Folds
    def foldLeft[B](z: B)(op: (B, A) => B)(implicit ec: EC): Future[B] = {
      def loop(xs: AsyncSeq[A], z: Future[B]): Future[B] = {
        xs.head flatMap {
          case Some(x) => loop(xs.tail, z.map(b => op(b, x)))
          case None    => z
        }
      }
      loop(xs, Future successful z)
    }

    def foldRight[B](z: B)(op: (A, B) => B)(implicit ec: EC): Future[B] = {
      xs.head flatMap {
        case Some(x) => xs.tail.foldRight(z)(op) map (b => op(x, b))
        case None    => Future successful z
      }
    }

    def fold[A1 >: A](z: A1)(op: (A1, A1) => A1)(implicit ec: EC): Future[A1] = foldLeft(z)(op)

    def reduceLeft[B >: A](op: (B, A) => B)(implicit ec: EC): Future[B] =
      xs.head flatMap {
        case Some(x) => tail.foldLeft[B](x)(op)
        case None    => Future failed new UnsupportedOperationException("empty.reduceLeft")
      }

    def reduceRight[B >: A](op: (A, B) => B)(implicit ec: EC): Future[B] =
      xs.head flatMap {
        case None    => Future failed new UnsupportedOperationException("empty.reduceRight")
        case Some(x) =>
          tail.isEmpty flatMap (if (_) Future successful x else tail.reduceRight(op) map (b => op(x, b)))
      }

    def reduce[A1 >: A](op: (A1, A1) => A1)(implicit ec: EC): Future[A1] = reduceLeft(op)
    def reduceLeftOption[B >: A](op: (B, A) => B): Future[Option[B]]     = ???
    def reduceRightOption[B >: A](op: (A, B) => B): Future[Option[B]]    = ???
    def reduceOption[A1 >: A](op: (A1, A1) => A1): Future[Option[A1]]    = reduceLeftOption(op)

    // Specific Folds
    def sum[A1 >: A](implicit num: Numeric[A1])        : Future[A1] = ???
    def product[A1 >: A](implicit num: Numeric[A1])    : Future[A1] = ???
    def min[A1 >: A](implicit ord: Ordering[A1])       : Future[A]  = ???
    def max[A1 >: A](implicit ord: Ordering[A1])       : Future[A]  = ???
    def minBy[B](f: A => B)(implicit cmp: Ordering[B]) : Future[A]  = ???
    def maxBy[B](f: A => B)(implicit cmp: Ordering[B]) : Future[A]  = ???

    def isTraversableAgain: Boolean = true

    def scan[B >: A](z: B)(op: (B, B) => B): AsyncSeq[B] = ???
    def scanLeft[B]( z: B)(op: (B, A) => B): AsyncSeq[B] = ???
    def scanRight[B](z: B)(op: (A, B) => B): AsyncSeq[B] = ???

    def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B] = AsyncSeq.flatten(xs)
 // def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B] = xs.foldLeft(AsyncSeq[B]())((b, a) => b ++ ev(a))

    def remove(p: A => Boolean): AsyncSeq[A] = ???

    // Copying
    def copyToBuffer[B >: A](xs: scala.collection.mutable.Buffer[B]): Unit                       = ???
    def copyToBuffer[B >: A](xs: scala.collection.mutable.Buffer[B], start: Int): Unit           = ???
    def copyToBuffer[B >: A](xs: scala.collection.mutable.Buffer[B], start: Int, len: Int): Unit = ???

    def copyToArray[B >: A](xs: Array[B]): Unit                       = ???
    def copyToArray[B >: A](xs: Array[B], start: Int): Unit           = ???
    def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = ???


    // Strings
    def mkString(start: String, sep: String, end: String): Future[String] = ???
    def mkString(sep: String): Future[String] = ???
    def mkString: Future[String] = ???

    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = ???
    def addString(b: StringBuilder, sep: String): StringBuilder = ???
    def addString(b: StringBuilder): StringBuilder = ???

 // override def toString: String = super.toString

    // Conversions
    def toArray[A1 >: A: ClassTag]: Future[Array[A1]] = ???
    def toList: Future[List[A]] = ???
    def toIndexedSeq: Future[scala.collection.immutable.IndexedSeq[A]] = ???
 // def toStream: Future[Stream[A]] = ???
    def toIterator: Future[Iterator[A]] = ???
    def toBuffer[A1 >: A]: Future[scala.collection.mutable.Buffer[A1]] = ???
    def toTraversable: Future[Traversable[A]] = ???
    def toIterable: Future[Iterable[A]] = ???
    def toSeq[A1 >: A]: Future[Seq[A]] = ???
    def toSet[A1 >: A]: Future[Set[A1]] = ???
    def toMap[K, V](implicit ev: A <:< (K, V)): Future[Map[K, V]] = ???
    def toVector(implicit ec: EC): Future[Vector[A]] = foldLeft(Vector.empty[A])(_ :+ _)
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A @uV]]): Col[A @uV] = ???

    def toStream: Stream[Future[Option[A]]] = {
      lazy val stream: Stream[AsyncSeq[A]] = Stream.cons(xs, stream.map(_.tail))
      stream.map(_.head)
    }
  }

  def apply[A](as: A*): AsyncSeq[A] = {
    if (as.isEmpty) ???
    else ???
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

  def tbd01[A](f: Future[AsyncSeq[A]]): AsyncSeq[A] = ???

  final class Seed[A] private[AsyncSeq] (fetch: A => Option[Future[A]])(implicit ec: EC) extends AsyncSeq[A] {
    private[AsyncSeq] val promise = Promise[Option[A]]()

    val head: Future[Option[A]] = promise.future

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
    private[AsyncSeq] val promise = Promise[Option[B]]()

    val head: Future[Option[B]] = promise.future

    lazy val tail = new Mapped(xs.tail, f)

    head.onSuccess {
      case Some(_) => tail.promise tryCompleteWith xs.tail.head.map(_ map f)
    }
  }

//  final class FlatMapped[A, B] private[AsyncSeq] (xs: AsyncSeq[A], f: A => AsyncSeq[B])(implicit ec: EC)
//    extends AsyncSeq[B]
//  {
//    private[AsyncSeq] val promise = Promise[Option[B]]()
//
//    val future: Future[Option[B]] = promise.future
//
//    lazy val tail = new FlatMapped(xs.tail, f)
//  }
}
