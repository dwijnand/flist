import flist._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._

implicit class AnyWith_>>[A](private val x: A) extends AnyVal {
  def >>() = println(x)
}

implicit class FutureWithAwait30s[A](private val future: Future[A]) extends AnyVal {
  def await30s =
    scala.concurrent.Await.result(future,
      scala.concurrent.duration.FiniteDuration(30, scala.concurrent.duration.SECONDS)
    )
}

final implicit class TravOnceWithMaxOpt[A](private val xs: TraversableOnce[A]) extends AnyVal {
  def maxOpt[B >: A](implicit cmp: Ordering[B]): Option[B] = if (xs.isEmpty) None else Some(xs max cmp)
}

final implicit class TravKVWithTabular[K, V](private val xs: Traversable[(K, V)]) extends AnyVal {
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.ToString")) // tabularisation is excused
  def maxKeyLen = xs.toIterator.map(_._1.toString.length).maxOpt
  def tabularkv = {
    xs.maxKeyLen.fold(Nil: Traversable[String]) { len =>
      val fmt = s"%${len}s %s"
      xs map (kv => fmt format(kv._1, kv._2))
    }
  }
  def showkv() = tabularkv foreach println
}

final implicit class TravKVsWithTabular[K, V](private val xs: Traversable[(K, Traversable[V])]) extends AnyVal {
  def tabularkvs = {
    xs.maxKeyLen.fold(Nil: Traversable[String]) { len =>
      val fmt = s"%${len}s %s"
      def showVs(vs: Traversable[V]) = if (vs.size == 1) vs.head else vs.mkString("[", ", ", "]")
      xs map (kv => fmt format(kv._1, showVs(kv._2)))
    }
  }
  def showkvs() = tabularkvs foreach println
}
