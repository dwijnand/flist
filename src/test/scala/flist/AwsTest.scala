package flist

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.util.Success
import scala.io.AnsiColor._

object AwsTest {
  val awsEndpointSleep = 200L

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits._

    val awsEndpoint = new FakeAwsEndpoint(awsEndpointSleep)

    val log1 = Log(s"${RED}v1$RESET")
    val log2 = Log(s"${GREEN}v2$RESET")

    val awsClient1 = new AwsClient1(awsEndpoint, log1)
    val awsClient2 = new AwsClient2(awsEndpoint, log2)

    val asgsAndLcs1: Future[Vector[(Asg, Lc)]] = v1(awsClient1)
    val asgsAndLcs2: FList[(Asg, Lc)] = v2Pre(awsClient2)

    asgsAndLcs1.await30s
    asgsAndLcs2.toVector.await30s

//    checkAndLog("v1", timedFuture(v1(awsClient1)).await30s)
//    checkAndLog("v2", timedFuture(v2(awsClient2)).await30s)
    ()
  }

  def v1(awsClient1: AwsClient1)(implicit ec: EC): Future[Vector[(Asg, Lc)]] = {
    val asgs       : Future[Vector[Asg]]       = awsClient1.getAsgs()
    val asgsAndLcs : Future[Vector[(Asg, Lc)]] =
      asgs flatMap { (asgs: Vector[Asg]) =>
        val asgsAndLcs: Future[Vector[(Asg, Lc)]] = awsClient1 getLcsForAsgs asgs
        asgsAndLcs
      }
    asgsAndLcs
  }

  def v2Pre(awsClient2: AwsClient2)(implicit ec: EC): FList[(Asg, Lc)] = {
    val asgs       : FList[Asg]       = awsClient2.getAsgs()
    val asgsAndLcs : FList[(Asg, Lc)] = awsClient2 getLcsForAsgs asgs
    asgsAndLcs
  }

  def v2(awsClient2: AwsClient2)(implicit ec: EC): Future[Vector[(Asg, Lc)]] = {
    val asgsAndLcs: Future[Vector[(Asg, Lc)]] = v2Pre(awsClient2).toVector
    asgsAndLcs
  }

  def checkAndLog(name: String, t: (Vector[(Asg, Lc)], Duration)): Unit = {
    val (asgsAndLcs, timed) = t
    println(s"timed: ${timed.toHHmmssSSS}")

    val expectedAsgs = AwsTestUtils makeAsgs (1 to 450)
    val expectedLcs  = AwsTestUtils makeLcs  (1 to 450)

    val expectedAsgsAndLcs = expectedAsgs zip expectedLcs

    if (asgsAndLcs != expectedAsgsAndLcs) {
      println(s"$name: asgs and lcs incoming don't match expected:")
      println(s"Expected:")
      expectedAsgsAndLcs foreach { case (asg, lc) => println(s"asg: $asg lc: $lc") }
      println(s"Incoming:")
      asgsAndLcs foreach { case (asg, lc) => println(s"asg: $asg lc: $lc") }
    }
  }

  def timedFuture[A](body: => Future[A])(implicit ec: EC): Future[(A, Duration)] = {
    val t0 = java.lang.System.nanoTime
    val x = body

    val p = Promise[Long]()
    x onComplete (_ => p complete Success(java.lang.System.nanoTime))

    x flatMap (res => p.future map (t1 => res -> (Duration fromNanos t1 - t0)))
  }
}
