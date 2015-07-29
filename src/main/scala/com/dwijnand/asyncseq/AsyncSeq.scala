package com.dwijnand.asyncseq

import scala.annotation.tailrec
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.reflect.ClassTag
import scala.util.{ Failure, Success }
import scala.{ PartialFunction => ?=> }

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

    def foreach[U](f: A => U): Future[Unit] = ???

    // Size info
    /** Tests whether this $coll is known to have a finite size.
      * It returns `'''true'''` if all elements have been computed.
      * It returns `'''false'''` if the sequence is not yet evaluated to the end.
      */
    // TODO: isAllCompleted?
    def hasDefiniteSize : Boolean         = ???
    def size            : Future[Int]     = ???
    def isEmpty         : Future[Boolean] = ???
    def nonEmpty        : Future[Boolean] = ???

    // Iterators
    def iterator: Future[Iterator[A]] = ???

    // Element Retrieval
    def head                  : Future[Option[A]] = xs.head
    def headForce             : Future[A]         = ???
    def last                  : Future[Option[A]] = ???
    def lastForce             : Future[A]         = ???
    def find(p: A => Boolean) : Future[Option[A]] = ???

    // Indexing and Length
    def apply(idx: Int): Future[Option[A]] = ???
    def isDefinedAt(idx: Int): Future[Boolean] = ???
    def length: Future[Int] = size
    def lengthCompare(ys: AsyncSeq[A]): Future[Int] = ???
    def indices: AsyncSeq[Int] = ???

    // Index Search
    def indexOf[A1 >: A](x: A1)                            : Future[Int] = ???
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
    def sortBy[B](f: A => B)(implicit ord: Ordering[B]) : AsyncSeq[A] = ???

    // Reversals
    def reverse: AsyncSeq[A] = ???
    def reverseIterator: Future[Iterator[A]] = ???
    def reverseMap[B](f: A => B): AsyncSeq[B] = ???

    // Multiset Operations
    def intersect[A1 >: A](ys: AsyncSeq[A1]): AsyncSeq[A] = ???
    def diff[A1 >: A](ys: AsyncSeq[A1]): AsyncSeq[A] = ???
    def union[A1 >: A](ys: AsyncSeq[A1]): AsyncSeq[A1] = xs ++ ys
    def distinct: AsyncSeq[A] = ???

    // Comparisons
    def sameElements[A1 >: A](ys: AsyncSeq[A1])               : Future[Boolean] = ???
    def startsWith[B](ys: AsyncSeq[B])                        : Future[Boolean] = ???
    def startsWith[B](ys: AsyncSeq[B], offset: Int)           : Future[Boolean] = ???
    def endsWith[B](ys: AsyncSeq[B])                          : Future[Boolean] = ???
    def contains[A1 >: A](elem: A1)                           : Future[Boolean] = ???
    def containsSlice[B](ys: AsyncSeq[B])                     : Future[Boolean] = ???
    def corresponds[B](ys: AsyncSeq[B])(p: (A, B) => Boolean) : Future[Boolean] = ???

    // Maps
    def map[B](f: A => B)(implicit ec: EC)               : AsyncSeq[B] = AsyncSeq.map(xs, f)
    def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC) : AsyncSeq[B] = AsyncSeq.flatMap(xs, f)
    def collect[B](pf: A ?=> B)                          : AsyncSeq[B] = ???

    // Subcollections
    def tail                         : AsyncSeq[A] = xs.tail
    def init                         : AsyncSeq[A] = ???
    def slice(from: Int, until: Int) : AsyncSeq[A] = ???
    def take(n: Int)                 : AsyncSeq[A] = ???
    def drop(n: Int)                 : AsyncSeq[A] = ???
    def takeWhile(p: A => Boolean)   : AsyncSeq[A] = ???
    def dropWhile(p: A => Boolean)   : AsyncSeq[A] = ???
    def takeRight(n: Int)            : AsyncSeq[A] = ???
    def dropRight(n: Int)            : AsyncSeq[A] = ???
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
    def forall(p: A => Boolean): Future[Boolean] = ???
    def exists(p: A => Boolean): Future[Boolean] = ???
    def count(p: A => Boolean): Future[Int]      = ???

    // Folds
    def foldLeft[B](z: B)(op: (B, A) => B): Future[B] = ???
    def foldRight[B](z: B)(op: (A, B) => B): Future[B] = ???
    def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): Future[A1] = foldLeft(z)(op)

    def reduceLeft[B >: A](op: (B, A) => B): Future[B]                = ???
    def reduceRight[B >: A](op: (A, B) => B): Future[B]               = ???
    def reduce[A1 >: A](op: (A1, A1) => A1): Future[A1]               = reduceLeft(op)
    def reduceLeftOption[B >: A](op: (B, A) => B): Future[Option[B]]  = ???
    def reduceRightOption[B >: A](op: (A, B) => B): Future[Option[B]] = ???
    def reduceOption[A1 >: A](op: (A1, A1) => A1): Future[Option[A1]] = reduceLeftOption(op)

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
    def toVector: Future[Vector[A]] = ???
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
