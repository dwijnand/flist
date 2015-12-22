package flist

import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.generic.{ CanBuildFrom => CBF }
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.concurrent.{ ExecutionContext => EC, Future }
import scala.reflect.{ ClassTag => CTag }
import scala.{ PartialFunction => ?=> }

abstract class FListOps[+A] {
  type FList[+X] <: FListOps[X]

  type This <: FList[A]
  type MapTo[+X] <: FList[X]

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

  def indices(implicit ec: EC): FList[Int] // 0 until length

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
  def indexOfSlice[A1 >: A](ys: FList[A1])            : Future[Int]
  def indexOfSlice[A1 >: A](ys: FList[A1], from: Int) : Future[Int]

  // Addition
  def  ++[A1 >: A](that: FList[A1])(implicit ec: EC): FList[A1]
  def ++:[A1 >: A](that: FList[A1])(implicit ec: EC): FList[A1]

  def +:[A1 >: A](x: A1)(implicit ec: EC): FList[A1]
  def :+[A1 >: A](x: A1)(implicit ec: EC): FList[A1]

  def padTo[A1 >: A](len: Int, x: A1)(implicit ec: EC): FList[A1]

  // Updates
  def patch[A1 >: A](from: Int, patch: FList[A1], replaced: Int): FList[A1]
  def updated[A1 >: A](idx: Int, x: A1): FList[A1]

  // Sorting
  def sorted[A1 >: A](implicit ord: Ordering[A1])     : FList[A]
  def sortWith(lt: (A, A) => Boolean)                 : FList[A]
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]) : FList[A]

  // Reversals
  def reverse: FList[A]
  def reverseIterator: Future[Iterator[A]]
  def reverseMap[B](f: A => B): FList[B]

  // Multiset Operations
  def union[A1 >: A](that: FList[A1])(implicit ec: EC): FList[A1]

  def intersect[A1 >: A](that: FList[A1]): FList[A]
  def diff     [A1 >: A](that: FList[A1]): FList[A]
  def distinct                              : FList[A]

  // Comparisons
  def sameElements[A1 >: A](that: FList[A1])(implicit ec: EC): Future[Boolean]

  def startsWith[B](that: FList[B])                        : Future[Boolean]
  def startsWith[B](that: FList[B], offset: Int)           : Future[Boolean]
  def endsWith[B](that: FList[B])                          : Future[Boolean]
  def contains[A1 >: A](x: A1)(implicit ec: EC)               : Future[Boolean]
  def containsSlice[B](that: FList[B])                     : Future[Boolean]
  def corresponds[B](that: FList[B])(p: (A, B) => Boolean) : Future[Boolean]

  // Maps
  def map[B](f: A => B)(implicit ec: EC): FList[B]

  def flatMap[B](f: A => FList[B])(implicit ec: EC) : FList[B]
  def collect[B](pf: A ?=> B)                          : FList[B]

  // Subcollections
  def tail                         : FList[A] // TODO: Option?
  def init                         : FList[A]
  def slice(from: Int, until: Int) : FList[A]

  def drop(n: Int)(implicit ec: EC): FList[A]

  def dropRight(n: Int): FList[A]

  def dropWhile(p: A => Boolean)(implicit ec: EC): FList[A]

  def take(n: Int)                 : FList[A]
  def takeRight(n: Int)            : FList[A]
  def takeWhile(p: A => Boolean)   : FList[A]
  def filter(   p: A => Boolean)   : FList[A]
  def filterNot(p: A => Boolean)   : FList[A]

  // Other iterators
  def grouped(size: Int)(implicit ec: EC): FList[FList[A]]

  def sliding(size: Int)            : FList[Vector[A]]
  def sliding(size: Int, step: Int) : FList[Vector[A]]

  // Zippers
  def zip[A1 >: A, B](ys: FList[B])                 : FList[(A1, B)]
  def zipAll[B, A1 >: A](ys: FList[B], a: A1, b: B) : FList[(A1, B)]
  def zipWithIndex[A1 >: A]                            : FList[(A1, Int)]

  // Subdivisions
  def splitAt(n: Int)            : (FList[A], FList[A])
  def span(p: A => Boolean)      : (FList[A], FList[A])
  def partition(p: A => Boolean) : (FList[A], FList[A])
  def groupBy[K](f: A => K)      : Future[Map[K, FList[A]]]

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

  def scan[B >: A](z: B)(op: (B, B) => B): FList[B]
  def scanLeft[B]( z: B)(op: (B, A) => B): FList[B]
  def scanRight[B](z: B)(op: (A, B) => B): FList[B]

  def flatten[B](implicit ec: EC, ev: A <:< FList[B]): FList[B]

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
