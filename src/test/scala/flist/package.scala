package flist

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import java.util.concurrent.TimeUnit

object `package` {
  def nanoToHHmmssSSS(nano: Long): String = {
    import TimeUnit._
    val hrs  = NANOSECONDS toHours   nano
    val mins = NANOSECONDS toMinutes nano - (HOURS toMillis hrs)
    val secs = NANOSECONDS toSeconds nano - (HOURS toMillis hrs) - (MINUTES toMillis mins)
    val ms   = NANOSECONDS toMillis  nano - (HOURS toMillis hrs) - (MINUTES toMillis mins) - (SECONDS toMillis secs)

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
