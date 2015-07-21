package com.dwijnand

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit

package object asyncseq {
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
}
