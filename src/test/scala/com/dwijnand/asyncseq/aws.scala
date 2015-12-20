package com.dwijnand.asyncseq

import scala.concurrent.duration._
import scala.concurrent.{ blocking, ExecutionContext => EC, Future }

final case class Asg(name: String, lcName: String)
final case class Lc(name: String)

final case class AsgReq(token: Option[Int] = None)
final case class AsgRsp(asgs: Vector[Asg], nextToken: Option[Int])

final case class LcReq(lcNames: Vector[String] = Vector.empty, token: Option[Int] = None)
final case class LcRsp(lcs: Vector[Lc], nextToken: Option[Int])

object AwsTestUtils {
  def makeAsgs(r: Range) = r.toVector.map(n => Asg(f"$n%03d", f"lc$n%03d"))
  def makeLcs( r: Range) = r.toVector.map(n => Lc(f"lc$n%03d"))
}
import AwsTestUtils._

final class FakeAwsEndpoint {
  val sleepMillis: Long = 1

  def describeAsgs(req: AsgReq)(implicit ec: EC): Future[AsgRsp] =
    Future {
      blocking {
        Thread sleep sleepMillis
        req.token match {
          case None    => asgRsp(  1 to  50, Some(2))
          case Some(2) => asgRsp( 51 to 100, Some(3))
          case Some(3) => asgRsp(101 to 150, Some(4))
          case Some(4) => asgRsp(151 to 200, Some(5))
          case Some(5) => asgRsp(201 to 250, Some(6))
          case Some(6) => asgRsp(251 to 300, Some(7))
          case Some(7) => asgRsp(301 to 350, Some(8))
          case Some(8) => asgRsp(351 to 400, Some(9))
          case Some(9) => asgRsp(401 to 450, None)
          case x       => sys error s"Unknown input $x"
        }
      }
    }

  def describeLcs(req: LcReq)(implicit ec: EC): Future[LcRsp] =
    Future {
      blocking {
        Thread sleep sleepMillis
        (req.lcNames, req.token) match {
          case (`lcns1`, None)    => lcRsp(  1 to  25, Some(2))
          case (`lcns1`, Some(2)) => lcRsp( 26 to  50, None)
          case (`lcns2`, None)    => lcRsp( 51 to  75, Some(2))
          case (`lcns2`, Some(2)) => lcRsp( 76 to 100, None)
          case (`lcns3`, None)    => lcRsp(101 to 125, Some(2))
          case (`lcns3`, Some(2)) => lcRsp(126 to 150, None)
          case (`lcns4`, None)    => lcRsp(151 to 175, Some(2))
          case (`lcns4`, Some(2)) => lcRsp(176 to 200, None)
          case (`lcns5`, None)    => lcRsp(201 to 225, Some(2))
          case (`lcns5`, Some(2)) => lcRsp(226 to 250, None)
          case (`lcns6`, None)    => lcRsp(251 to 275, Some(2))
          case (`lcns6`, Some(2)) => lcRsp(276 to 300, None)
          case (`lcns7`, None)    => lcRsp(301 to 325, Some(2))
          case (`lcns7`, Some(2)) => lcRsp(326 to 350, None)
          case (`lcns8`, None)    => lcRsp(351 to 375, Some(2))
          case (`lcns8`, Some(2)) => lcRsp(376 to 400, None)
          case (`lcns9`, None)    => lcRsp(401 to 425, Some(2))
          case (`lcns9`, Some(2)) => lcRsp(426 to 450, None)
          case x                  => sys error s"Unknown input $x"
        }
      }
    }

  private def asgRsp(r: Range, nextToken: Option[Int]) = AsgRsp(makeAsgs(r), nextToken)
  private def lcRsp( r: Range, nextToken: Option[Int]) = LcRsp( makeLcs(r),  nextToken)

  private def lcNames(r: Range) = r.toVector.map(n => f"lc$n%03d")

  private val lcns1 = lcNames(  1 to 50)
  private val lcns2 = lcNames( 51 to 100)
  private val lcns3 = lcNames(101 to 150)
  private val lcns4 = lcNames(151 to 200)
  private val lcns5 = lcNames(201 to 250)
  private val lcns6 = lcNames(251 to 300)
  private val lcns7 = lcNames(301 to 350)
  private val lcns8 = lcNames(351 to 400)
  private val lcns9 = lcNames(401 to 450)
}

final class AwsClient(awsEndpoint: FakeAwsEndpoint) {
  def getAsgsPage(req: AsgReq = AsgReq())(implicit ec: EC): Future[AsgRsp] = awsEndpoint describeAsgs req
  def getLcsPage( req: LcReq  = LcReq()) (implicit ec: EC): Future[LcRsp]  = awsEndpoint describeLcs req

  def getAsgs1(req: AsgReq = AsgReq())(implicit ec: EC): Future[Vector[Asg]] = {
    def loop(req: AsgReq, asgs: Vector[Asg]): Future[Vector[Asg]] =
      getAsgsPage(req) flatMap { rsp =>
        rsp.nextToken match {
          case None => Future successful asgs ++ rsp.asgs
          case t    => loop(req.copy(token = t), asgs ++ rsp.asgs)
        }
      }
    loop(req, Vector.empty)
  }

  def getLcs1(req: LcReq = LcReq())(implicit ec: EC): Future[Vector[Lc]] = {
    def loop(req: LcReq, lcs: Vector[Lc]): Future[Vector[Lc]] =
      getLcsPage(req) flatMap { rsp =>
        rsp.nextToken match {
          case None => Future successful lcs ++ rsp.lcs
          case t    => loop(req.copy(token = t), lcs ++ rsp.lcs)
        }
      }
    loop(req, Vector.empty)
  }

  def getLcsForAsgs1(asgs: Vector[Asg])(implicit ec: EC): Future[Vector[(Asg, Lc)]] =
    Future
      .traverse(asgs grouped 50) { asgs =>
        getLcs1(LcReq(asgs map (_.lcName)))
          .map(_.map(lc => lc.name -> lc).toMap)
          .map { lcsMap =>
            asgs flatMap { asg =>
              lcsMap get asg.lcName match {
                case Some(lc) => Seq((asg, lc))
                case None     => println(s"Dropping asg with no lc: $asg"); Nil
              }
            }
          }
      }
      .map(_.flatten.toVector)

  def getAsgs2(req: AsgReq = AsgReq())(implicit ec: EC): AsyncSeq[Asg] =
    AsyncSeq
      .unpaginate(getAsgsPage(req))(asgRsp => asgRsp.nextToken map (t => getAsgsPage(req.copy(token = Some(t)))))
      .flatMap(asgRsp => AsyncSeq fromSeq asgRsp.asgs)

  def getLcs2(req: LcReq = LcReq())(implicit ec: EC): AsyncSeq[Lc] =
    AsyncSeq
      .unpaginate(getLcsPage(req))(lcRsp => lcRsp.nextToken map (t => getLcsPage(req.copy(token = Some(t)))))
      .flatMap(lcRsp => AsyncSeq fromSeq lcRsp.lcs)

  def getLcsForAsgs2(asgs: AsyncSeq[Asg])(implicit ec: EC): AsyncSeq[(Asg, Lc)] = {
    asgs
      .grouped(50)
      .flatMap { asgs =>
        AsyncSeq fromFuture asgs.toVector.flatMap { asgs =>
          getLcs2(LcReq(asgs map (_.lcName))).map(lc => lc.name -> lc).toMap map { lcMap =>
            asgs flatMap { asg =>
              lcMap get asg.lcName match {
                case Some(lc) => Seq((asg, lc))
                case None     => println(s"Dropping asg with no lc: $asg"); Nil
              }
            }
          } map AsyncSeq.fromSeq
        }
      }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits._

    val awsClient = new AwsClient(new FakeAwsEndpoint)

    checkAndLog(TimedFuture(awsClient.getAsgs1() flatMap awsClient.getLcsForAsgs1).await30s)
    checkAndLog(TimedFuture(awsClient.getLcsForAsgs2(awsClient.getAsgs2()).toVector).await30s)
  }

  def checkAndLog(t: (Vector[(Asg, Lc)], Duration)): Unit = {
    val (asgsAndLcs, timed) = t
    println(s"timed: ${timed.toHHmmssSSS}")

    val expectedAsgs = makeAsgs(1 to 450)
    val expectedLcs  = makeLcs (1 to 450)

    if (asgsAndLcs != (expectedAsgs zip expectedLcs)) {
      println(s"asgs and lcs expected don't match actual:")
      asgsAndLcs foreach { case (asg, lc) => println(s"asg: $asg  lc: $lc") }
    }
  }
}
