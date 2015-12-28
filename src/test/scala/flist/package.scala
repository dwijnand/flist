package flist

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import java.util.concurrent.TimeUnit

object `package` {
  def nanoToHHmmssSSS(nano: Long): String = {
    import TimeUnit._
    val l = NANOSECONDS toMillis nano
    val hrs  = MILLISECONDS toHours   l
    val mins = MILLISECONDS toMinutes l - (HOURS toMillis hrs)
    val secs = MILLISECONDS toSeconds l - (HOURS toMillis hrs) - (MINUTES toMillis mins)
    val ms   = MILLISECONDS toMillis  l - (HOURS toMillis hrs) - (MINUTES toMillis mins) - (SECONDS toMillis secs)

    f"$hrs%02dh$mins%02dm$secs%02ds$ms%03d"
  }

  implicit final class DurationW(private val d: Duration) extends AnyVal {
    def toHHmmssSSS = nanoToHHmmssSSS(d.toNanos)
  }
  implicit final class FutureWithAwait30s[A](private val fut: Future[A]) extends AnyVal {
    def await(d: Duration) = Await.result(fut, d)
    def await30s           = fut await 30.seconds
  }
}
