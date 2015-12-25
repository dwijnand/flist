package flist

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.Success

object TimedFuture {
  def apply[A](body: => Future[A])(implicit ec: ExecutionContext): Future[(A, Duration)] = {
    val t0 = java.lang.System.nanoTime
    val x = body

    val p = Promise[Long]()
    x onComplete (_ => p complete Success(java.lang.System.nanoTime))

    x flatMap (res => p.future map (t1 => res -> (Duration fromNanos t1 - t0)))
  }
}
