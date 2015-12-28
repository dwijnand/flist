package flist

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
        FList fromFutureSeq asgs.toVector.flatMap { asgs =>
          getLcs(LcReq(asgs map (_.lcName))).map(lc => lc.name -> lc).toMap map { lcMap =>
            asgs flatMap { asg =>
              lcMap get asg.lcName match {
                case Some(lc) => Seq((asg, lc))
                case None     => println(s"Dropping asg with no lc: $asg"); Nil
              }
            }
          }
        }
      }
  }
}
