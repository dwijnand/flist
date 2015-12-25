package flist

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext => EC, Future }

final class AwsClient1(awsEndpoint: FakeAwsEndpoint) {
  def getAsgsPage(req: AsgReq = AsgReq())(implicit ec: EC): Future[AsgRsp] = awsEndpoint describeAsgs req
  def getLcsPage( req: LcReq  = LcReq()) (implicit ec: EC): Future[LcRsp]  = awsEndpoint describeLcs req

  def getAsgs(req: AsgReq = AsgReq())(implicit ec: EC): Future[Vector[Asg]] = {
    def loop(req: AsgReq, asgs: Vector[Asg]): Future[Vector[Asg]] =
      getAsgsPage(req) flatMap { rsp =>
        rsp.nextToken match {
          case None => Future successful asgs ++ rsp.asgs
          case t    => loop(req.copy(token = t), asgs ++ rsp.asgs)
        }
      }
    loop(req, Vector.empty)
  }

  def getLcs(req: LcReq = LcReq())(implicit ec: EC): Future[Vector[Lc]] = {
    def loop(req: LcReq, lcs: Vector[Lc]): Future[Vector[Lc]] =
      getLcsPage(req) flatMap { rsp =>
        rsp.nextToken match {
          case None => Future successful lcs ++ rsp.lcs
          case t    => loop(req.copy(token = t), lcs ++ rsp.lcs)
        }
      }
    loop(req, Vector.empty)
  }

  def getLcsForAsgs(asgs: Vector[Asg])(implicit ec: EC): Future[Vector[(Asg, Lc)]] =
    Future
      .traverse(asgs grouped 50) { asgs =>
        getLcs(LcReq(asgs map (_.lcName)))
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
}

final class AwsClient2(awsEndpoint: FakeAwsEndpoint) {
  def getAsgsPage(req: AsgReq = AsgReq())(implicit ec: EC): Future[AsgRsp] = awsEndpoint describeAsgs req
  def getLcsPage( req: LcReq  = LcReq()) (implicit ec: EC): Future[LcRsp]  = awsEndpoint describeLcs req

  def getAsgs(req: AsgReq = AsgReq())(implicit ec: EC): FList[Asg] =
    FList
      .unpaginate(getAsgsPage(req))(asgRsp => asgRsp.nextToken map (t => getAsgsPage(req.copy(token = Some(t)))))
      .flatMap(asgRsp => FList fromSeq asgRsp.asgs)

  def getLcs(req: LcReq = LcReq())(implicit ec: EC): FList[Lc] =
    FList
      .unpaginate(getLcsPage(req))(lcRsp => lcRsp.nextToken map (t => getLcsPage(req.copy(token = Some(t)))))
      .flatMap(lcRsp => FList fromSeq lcRsp.lcs)

  def getLcsForAsgs(asgs: FList[Asg])(implicit ec: EC): FList[(Asg, Lc)] = {
    asgs
      .grouped(50)
      .flatMap { asgs =>
        FList fromFuture asgs.toVector.flatMap { asgs =>
          getLcs(LcReq(asgs map (_.lcName))).map(lc => lc.name -> lc).toMap map { lcMap =>
            asgs flatMap { asg =>
              lcMap get asg.lcName match {
                case Some(lc) => Seq((asg, lc))
                case None     => println(s"Dropping asg with no lc: $asg"); Nil
              }
            }
          } map FList.fromSeq
        }
      }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits._

    val awsClient1 = new AwsClient1(new FakeAwsEndpoint(1))
    val awsClient2 = new AwsClient2(new FakeAwsEndpoint(1))

    checkAndLog(TimedFuture(awsClient1.getAsgs() flatMap awsClient1.getLcsForAsgs).await30s)
    checkAndLog(TimedFuture(awsClient2.getLcsForAsgs(awsClient2.getAsgs()).toVector).await30s)
  }

  def checkAndLog(t: (Vector[(Asg, Lc)], Duration)): Unit = {
    val (asgsAndLcs, timed) = t
    println(s"timed: ${timed.toHHmmssSSS}")

    val expectedAsgs = AwsTestUtils makeAsgs (1 to 450)
    val expectedLcs  = AwsTestUtils makeLcs  (1 to 450)

    if (asgsAndLcs != (expectedAsgs zip expectedLcs)) {
      println(s"asgs and lcs expected don't match actual:")
      asgsAndLcs foreach { case (asg, lc) => println(s"asg: $asg  lc: $lc") }
    }
  }
}
