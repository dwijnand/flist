package flist

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext => EC, Future, Promise }
import scala.util.Success

object AwsTest {
  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits._

    val awsEndpoint: FakeAwsEndpoint = new FakeAwsEndpoint(1)
    val awsClient1 = new AwsClient1(awsEndpoint)
    val awsClient2 = new AwsClient2(awsEndpoint)

    checkAndLog("v1", timedFuture(awsClient1.getAsgs() flatMap awsClient1.getLcsForAsgs).await30s)
    checkAndLog("v2", timedFuture(awsClient2.getLcsForAsgs(awsClient2.getAsgs()).toVector).await30s)
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
