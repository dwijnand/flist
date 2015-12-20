package asyncseq

import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.{ CanBuildFrom => CBF }
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.concurrent.{ ExecutionContext => EC, Future }
import scala.reflect.{ ClassTag => CTag }
import scala.{ PartialFunction => ?=> }

abstract class AsyncSeqOps[+A] {
  type AsyncSeq[+X] <: AsyncSeqOps[X]
  type This <: AsyncSeq[A]

  // Foreach
  def foreach[U](f: A => U)(implicit ec: EC): Future[Unit]

  // Size info
  def hasDefiniteSize: Boolean
  def isTraversableAgain: Boolean

  def isEmpty (implicit ec: EC): Future[Boolean]
  def nonEmpty(implicit ec: EC): Future[Boolean]
  def size    (implicit ec: EC): Future[Int]
  def length  (implicit ec: EC): Future[Int]

  // Iterators
  def iterator(implicit ec: EC): Future[Iterator[A]]

  // Element Retrieval
  def head: Future[Option[A]]

  def last(implicit ec: EC): Future[Option[A]]
  def find(p: A => Boolean)(implicit ec: EC): Future[Option[A]]

  // Indexing and Length
  def apply(idx: Int)(implicit ec: EC): Future[A] // TODO: Future[Option[A]] ?

  def isDefinedAt(idx: Int)(implicit ec: EC): Future[Boolean]

  def lengthCompare(len: Int)(implicit ec: EC): Future[Int]

  def indices(implicit ec: EC): AsyncSeq[Int] // 0 until length

  // Index Search
  def indexOf[A1 >: A](x: A1)                            : Future[Int]
  def indexOf[A1 >: A](x: A1, from: Int)                 : Future[Int]
  def lastIndexOf[A1 >: A](x: A1)                        : Future[Int]
  def lastIndexOf[A1 >: A](x: A1, end: Int)              : Future[Int]
  def indexWhere(p: A => Boolean, from: Int)             : Future[Int]
  def indexWhere(p: A => Boolean)                        : Future[Int]
  def lastIndexWhere[A1 >: A](x: A1)                     : Future[Int]
  def lastIndexWhere[A1 >: A](x: A1, end: Int)           : Future[Int]
  def segmentLength(p: A => Boolean, from: Int)          : Future[Int]
  def prefixLength(p: A => Boolean)                      : Future[Int]
  def indexOfSlice[A1 >: A](ys: AsyncSeq[A1])            : Future[Int]
  def indexOfSlice[A1 >: A](ys: AsyncSeq[A1], from: Int) : Future[Int]

  // Addition
  def  ++[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): AsyncSeq[A1]
  def ++:[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): AsyncSeq[A1]

  def +:[A1 >: A](x: A1)(implicit ec: EC): AsyncSeq[A1]
  def :+[A1 >: A](x: A1)(implicit ec: EC): AsyncSeq[A1]

  def padTo[A1 >: A](len: Int, x: A1)(implicit ec: EC): AsyncSeq[A1]

  // Updates
  def patch[A1 >: A](from: Int, patch: AsyncSeq[A1], replaced: Int): AsyncSeq[A1]
  def updated[A1 >: A](idx: Int, x: A1): AsyncSeq[A1]

  // Sorting
  def sorted[A1 >: A](implicit ord: Ordering[A1])     : AsyncSeq[A]
  def sortWith(lt: (A, A) => Boolean)                 : AsyncSeq[A]
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]) : AsyncSeq[A]

  // Reversals
  def reverse: AsyncSeq[A]
  def reverseIterator: Future[Iterator[A]]
  def reverseMap[B](f: A => B): AsyncSeq[B]

  // Multiset Operations
  def union[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): AsyncSeq[A1]

  def intersect[A1 >: A](that: AsyncSeq[A1]): AsyncSeq[A]
  def diff     [A1 >: A](that: AsyncSeq[A1]): AsyncSeq[A]
  def distinct                              : AsyncSeq[A]

  // Comparisons
  def sameElements[A1 >: A](that: AsyncSeq[A1])(implicit ec: EC): Future[Boolean]

  def startsWith[B](that: AsyncSeq[B])                        : Future[Boolean]
  def startsWith[B](that: AsyncSeq[B], offset: Int)           : Future[Boolean]
  def endsWith[B](that: AsyncSeq[B])                          : Future[Boolean]
  def contains[A1 >: A](x: A1)(implicit ec: EC)               : Future[Boolean]
  def containsSlice[B](that: AsyncSeq[B])                     : Future[Boolean]
  def corresponds[B](that: AsyncSeq[B])(p: (A, B) => Boolean) : Future[Boolean]

  // Maps
  def map[B](f: A => B)(implicit ec: EC): AsyncSeq[B]

  def flatMap[B](f: A => AsyncSeq[B])(implicit ec: EC) : AsyncSeq[B]
  def collect[B](pf: A ?=> B)                          : AsyncSeq[B]

  // Subcollections
  def tail                         : AsyncSeq[A] // TODO: Option?
  def init                         : AsyncSeq[A]
  def slice(from: Int, until: Int) : AsyncSeq[A]

  def drop(n: Int)(implicit ec: EC): AsyncSeq[A]

  def dropRight(n: Int): AsyncSeq[A]

  def dropWhile(p: A => Boolean)(implicit ec: EC): AsyncSeq[A]

  def take(n: Int)                 : AsyncSeq[A]
  def takeRight(n: Int)            : AsyncSeq[A]
  def takeWhile(p: A => Boolean)   : AsyncSeq[A]
  def filter(   p: A => Boolean)   : AsyncSeq[A]
  def filterNot(p: A => Boolean)   : AsyncSeq[A]

  // Other iterators
  def grouped(size: Int)(implicit ec: EC): AsyncSeq[AsyncSeq[A]]

  def sliding(size: Int)            : AsyncSeq[Vector[A]]
  def sliding(size: Int, step: Int) : AsyncSeq[Vector[A]]

  // Zippers
  def zip[A1 >: A, B](ys: AsyncSeq[B])                 : AsyncSeq[(A1, B)]
  def zipAll[B, A1 >: A](ys: AsyncSeq[B], a: A1, b: B) : AsyncSeq[(A1, B)]
  def zipWithIndex[A1 >: A]                            : AsyncSeq[(A1, Int)]

  // Subdivisions
  def splitAt(n: Int)            : (AsyncSeq[A], AsyncSeq[A])
  def span(p: A => Boolean)      : (AsyncSeq[A], AsyncSeq[A])
  def partition(p: A => Boolean) : (AsyncSeq[A], AsyncSeq[A])
  def groupBy[K](f: A => K)      : Future[Map[K, AsyncSeq[A]]]

  // Element Conditions
  def forall(p: A => Boolean)(implicit ec: EC): Future[Boolean]
  def exists(p: A => Boolean)(implicit ec: EC): Future[Boolean]
  def count(p: A => Boolean)(implicit ec: EC): Future[Int]

  // Folds
  def fold  [A1 >: A](z: A1)(op: (A1, A1) => A1)(implicit ec: EC): Future[A1]
  def reduce[A1 >: A]       (op: (A1, A1) => A1)(implicit ec: EC): Future[Option[A1]]

  def foldLeft[B](z: B)(op: (B, A) => B)(implicit ec: EC): Future[B]
  def foldRight[B](z: B)(op: (A, B) => B)(implicit ec: EC): Future[B]

  def reduceLeft[A1 >: A](op: (A1, A) => A1)(implicit ec: EC): Future[Option[A1]]
  def reduceRight[A1 >: A](op: (A, A1) => A1)(implicit ec: EC): Future[Option[A1]]

  // Specific Folds
  def sum    [A1 >: A](implicit num: Numeric[A1], ec: EC): Future[A1]
  def product[A1 >: A](implicit num: Numeric[A1], ec: EC): Future[A1]

  def min[A1 >: A](implicit ord: Ordering[A1], ec: EC): Future[Option[A]]

  def max[A1 >: A](implicit ord: Ordering[A1], ec: EC): Future[Option[A]]

  def minBy[B](f: A => B)(implicit ord: Ordering[B], ec: EC): Future[Option[A]]
  def maxBy[B](f: A => B)(implicit ord: Ordering[B], ec: EC): Future[Option[A]]

  def scan[B >: A](z: B)(op: (B, B) => B): AsyncSeq[B]
  def scanLeft[B]( z: B)(op: (B, A) => B): AsyncSeq[B]
  def scanRight[B](z: B)(op: (A, B) => B): AsyncSeq[B]

  def flatten[B](implicit ec: EC, ev: A <:< AsyncSeq[B]): AsyncSeq[B]

  // Copying
  def copyToBuffer[B >: A](xs: mutable.Buffer[B])(implicit ec: EC): Future[Unit]

  def copyToArray[B >: A](arr: Array[B])                      (implicit ec: EC): Future[Unit]
  def copyToArray[B >: A](arr: Array[B], start: Int)          (implicit ec: EC): Future[Unit]
  def copyToArray[B >: A](arr: Array[B], start: Int, len: Int)(implicit ec: EC): Future[Unit]

  // Conversions
  def to[Col[_]](implicit cbf: CBF[Nothing, A, Col[A @uV]], ec: EC): Future[Col[A @uV]]

  def toArray [A1 >: A: CTag](implicit ec: EC): Future[Array[A1]]
  def toBuffer[A1 >: A]      (implicit ec: EC): Future[mutable.Buffer[A1]]
  def toSet   [A1 >: A]      (implicit ec: EC): Future[Set[A1]]

  def toList  (implicit ec: EC): Future[List[A]]
  def toStream(implicit ec: EC): Future[Stream[A]]
  def toVector(implicit ec: EC): Future[Vector[A]]

  def toMap[K, V](implicit ec: EC, ev: A <:< (K, V)): Future[Map[K, V]]

  def toIterator   (implicit ec: EC): Future[Iterator[A]]
  def toIndexedSeq (implicit ec: EC): Future[IndexedSeq[A]]
  def toSeq        (implicit ec: EC): Future[Seq[A]]
  def toIterable   (implicit ec: EC): Future[Iterable[A]]
  def toTraversable(implicit ec: EC): Future[Traversable[A]]

  def toStreamFuture: Stream[Future[Option[A]]]

  // Strings
  def mkString             : String
  def mkString(sep: String): String

  def mkString(start: String, sep: String, end: String): String

  def addString(b: StringBuilder)             : StringBuilder
  def addString(b: StringBuilder, sep: String): StringBuilder

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder

  override def toString: String
}
