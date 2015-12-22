package asyncseq

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.io.AnsiColor._

object Test {
  val   allocSleep = 800L // simulating some time to do some IO
  val inspectSleep = 2200L // a longer time to test that seeding, mapping & flat-mapping happens asynchronously

  def isoTime() = {
    val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    df setTimeZone (java.util.TimeZone getTimeZone "UTC")
    df format new java.util.Date()
  }

  def alloc(n: Int): Future[Int] =
    Future {
      println(s"$isoTime $CYAN[2]$RESET allocating $n...")
      Thread sleep allocSleep
      println(s"$isoTime $CYAN[2]$RESET allocated  $n")
      n
    }

  def createPayload: AsyncSeq[Int] = {
    println(s"$isoTime $WHITE[1]$RESET creating AsyncSeq...")
    val payload = AsyncSeq.unpag[Int](alloc(0), n => if (n < 9) Some(alloc(n + 1)) else None)
    println(s"$isoTime $WHITE[1]$RESET created  AsyncSeq")
    payload
  }

  @tailrec def inspectPayload[A](p: AsyncSeq[A]): Unit = {
    Await.result(p.value, 2.seconds) match {
      case (a, Some(tail)) =>
        println(s"$isoTime $YELLOW[4]$RESET saw $a")
        Thread sleep inspectSleep
        inspectPayload(tail)
      case (a, None) => println(s"$isoTime $YELLOW[4]$RESET saw $a (last)")
    }
  }

  def main(args: Array[String]): Unit = {
//    inspectPayload(createPayload)
//    println()
//    inspectPayload(createPayload map { n =>
//      println(s"$isoTime $MAGENTA[3]$RESET transforming $n to Double")
//      n.toDouble
//    })
//    println()
//    inspectPayload(createPayload flatMap { n =>
//      println(s"$isoTime $MAGENTA[3]$RESET duplicating $n into two")
//      Cons(n, Future(Last(n)))
//    })
  }
}
