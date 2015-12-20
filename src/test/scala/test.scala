package asyncseq

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.io.AnsiColor._

object Test {
  def alloc(n: Int): Int = {
    println(s"$CYAN[2]$RESET allocating $n...")
    Thread sleep 10 // simulating some time to do some IO
    println(s"$CYAN[2]$RESET allocated  $n")
    n
  }

  def createPayload: AsyncSeq[Int] = {
    println(s"$WHITE[1]$RESET creating AsyncSeq...")
    val payload = AsyncSeq[Int](alloc(0), n => if (n < 9) Some(Future(alloc(n + 1))) else None)
    println(s"$WHITE[1]$RESET created  AsyncSeq")
    payload
  }

  @tailrec def inspectPayload[A](p: AsyncSeq[A]): Unit =
    p match {
      case Last(n)     =>
        println(s"$YELLOW[4]$RESET saw $n (last)")
      case Cons(n, ft) =>
        println(s"$YELLOW[4]$RESET saw $n")
        Thread sleep 80 // a longer time to test that seeding, mapping & flat-mapping happens asynchronously
        inspectPayload(Await.result(ft, 2.seconds))
    }

  def main(args: Array[String]): Unit = {
    inspectPayload(createPayload)
    println()
    inspectPayload(createPayload map { n =>
      println(s"$MAGENTA[3]$RESET transforming $n to Double")
      n.toDouble
    })
    println()
    inspectPayload(createPayload flatMap { n =>
      println(s"$MAGENTA[3]$RESET duplicating $n into two")
      Cons(n, Future(Last(n)))
    })
  }
}
