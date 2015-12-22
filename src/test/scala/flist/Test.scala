package flist

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.io.AnsiColor._

object Test {
  val     maxAlloc = 100
  val   allocSleep = 10L // simulating some time to do some IO
  val inspectSleep = 80L // a longer time to test that seeding, mapping & flat-mapping happens asynchronously

  def isoTime() = {
    val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    df setTimeZone (java.util.TimeZone getTimeZone "UTC")
    df format new java.util.Date()
  }

  def alloc(n: Int): Future[Option[Int]] =
    Future {
      println(s"$isoTime $CYAN[2]$RESET allocating $n...")
      Thread sleep allocSleep
      println(s"$isoTime $CYAN[2]$RESET allocated  $n")
      Some(n)
    }

  def createPayload: FList[Int] = {
    println(s"$isoTime $WHITE[1]$RESET creating AsyncSeq...")
    val payload = FList.iterate[Int](alloc(0))(n => if (n < maxAlloc) alloc(n + 1) else Future successful None)
    println(s"$isoTime $WHITE[1]$RESET created  AsyncSeq")
    payload
  }

  @tailrec def inspectPayload[A](p: FList[A]): Unit = {
    Await.result(p.value.value, 2.seconds) match {
      case Some((a, tail)) =>
        println(s"$isoTime $YELLOW[4]$RESET saw $a")
        Thread sleep inspectSleep
        inspectPayload(tail)
      case None => println(s"$isoTime $YELLOW[4]$RESET saw last")
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
//      FList(FutureOption(Future(Some((n,
//        FList(FutureOption(Future(Some((n,
//          FList(FutureOption(Future(None)))))))))))))
//    })
  }
}
