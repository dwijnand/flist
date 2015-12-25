package flist

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import java.util.concurrent.TimeUnit

object `package` {
  implicit final class DurationW(private val d: Duration) extends AnyVal {
    def toHHmmssSSS = {
      import TimeUnit._
      val l = d.toMillis

      val hrs  = MILLISECONDS toHours   l
      val mins = MILLISECONDS toMinutes l - (HOURS toMillis hrs)
      val secs = MILLISECONDS toSeconds l - (HOURS toMillis hrs) - (MINUTES toMillis mins)
      val ms   = MILLISECONDS toMillis  l - (HOURS toMillis hrs) - (MINUTES toMillis mins) - (SECONDS toMillis secs)

      f"$hrs%02dh$mins%02dm$secs%02ds$ms%03d"
    }
  }
  implicit final class FutureWithAwait30s[A](private val fut: Future[A]) extends AnyVal {
    def await30s = Await.result(fut, 30.seconds)
  }
}
