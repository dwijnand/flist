package com.dwijnand.asyncseq

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }


final case class Asg(name: String, launchConfigName: String)

final case class AsgReq(token: Option[Int] = None)
final case class AsgRes(asgs: Vector[Asg], nextToken: Option[Int])


final case class LaunchConfig(name: String)

final case class LaunchConfigReq(launchConfigNames: Vector[String] = Vector.empty, token: Option[Int] = None)
final case class LaunchConfigRes(launchConfigs: Vector[LaunchConfig], nextToken: Option[Int])


object Main {
  def main(args: Array[String]): Unit = {
    val asgClient = new AsgClient
    val awsClient = new AwsClient(asgClient)
    import scala.concurrent.ExecutionContext.Implicits.global
    val (asgsAndLaunchConfigs: Vector[(Asg, LaunchConfig)], timed: Duration) =
      TimedFuture {
        awsClient.getAsgs() flatMap (asgs => awsClient.getLaunchConfigsForAsgs(asgs))
      }.await30s
    println(s"timed: ${timed.toHHmmssSSS}")
    println(s"asgs and launch configs:")
    asgsAndLaunchConfigs foreach { case (asg, launchConfig) =>
      println(s"asg: $asg  launch config: $launchConfig")
    }
  }
}

final class AwsClient(asgClient: AsgClient) {
  def getAsgsPage(req: AsgReq = AsgReq())(implicit ec: ExecutionContext)
  : Future[AsgRes] =
    asgClient describeAsgsAsync req

  def getLaunchConfigsPage(req: LaunchConfigReq = LaunchConfigReq())(implicit ec: ExecutionContext)
  : Future[LaunchConfigRes] =
    asgClient describeLaunchConfigs req

  def getAsgs(req: AsgReq = AsgReq())(implicit ec: ExecutionContext) = {
    def loop(req: AsgReq, asgs: Vector[Asg]): Future[Vector[Asg]] =
      getAsgsPage(req).flatMap { res =>
        val allAsgs = asgs ++ res.asgs
        res.nextToken match {
          case None      => Future successful allAsgs
          case nextToken => loop(req.copy(token = nextToken), allAsgs)
        }
      }
    loop(req, Vector.empty)
  }

  def getLaunchConfigs(req: LaunchConfigReq = LaunchConfigReq())(implicit ec: ExecutionContext)
  : Future[Vector[LaunchConfig]] = {
    def loop(req: LaunchConfigReq, launchConfigs: Vector[LaunchConfig]): Future[Vector[LaunchConfig]] =
      getLaunchConfigsPage(req).flatMap { res =>
        val allLaunchConfigs = launchConfigs ++ res.launchConfigs
        res.nextToken match {
          case None      => Future successful allLaunchConfigs
          case nextToken => loop(req.copy(token = nextToken), allLaunchConfigs)
        }
      }
    loop(req, Vector.empty)
  }

  def getLaunchConfigsForAsgs(asgs: Vector[Asg])(implicit ec: ExecutionContext)
  : Future[Vector[(Asg, LaunchConfig)]] =
    Future
      .traverse(asgs grouped 50) { asgs =>
        getLaunchConfigs(LaunchConfigReq(asgs.map(_.launchConfigName)))
          .map(_.map(launchConfig => launchConfig.name -> launchConfig).toMap)
          .map { launchConfigs =>
            asgs flatMap { asg =>
              launchConfigs.get(asg.launchConfigName) match {
                case Some(launchConfig) => Some((asg, launchConfig))
                case None               => println(s"Dropping asg with no launch config: $asg"); None
              }
            }
          }
      }
      .map(_.flatten.toVector)
}

final class AsgClient {
  def describeAsgsAsync(req: AsgReq)(implicit ec: ExecutionContext): Future[AsgRes] =
    Future {
      Thread sleep 10
      req.token match {
        case None    => asgRes(  1 to  50, Some(2))
        case Some(2) => asgRes( 51 to 100, Some(3))
        case Some(3) => asgRes(101 to 150, Some(4))
        case Some(4) => asgRes(151 to 200, Some(5))
        case Some(5) => asgRes(201 to 250, Some(6))
        case Some(6) => asgRes(251 to 300, Some(7))
        case Some(7) => asgRes(301 to 350, Some(8))
        case Some(8) => asgRes(351 to 400, Some(9))
        case Some(9) => asgRes(401 to 450, None)
        case x       => sys error s"Unknown input $x"
      }
    }

  def describeLaunchConfigs(req: LaunchConfigReq)(implicit ec: ExecutionContext): Future[LaunchConfigRes] =
    Future {
      Thread sleep 100
      (req.launchConfigNames, req.token) match {
        case (`lcns1`, None)    => launchConfigRes(  1 to  25, Some(2))
        case (`lcns1`, Some(2)) => launchConfigRes( 26 to  50, None)
        case (`lcns2`, None)    => launchConfigRes( 51 to  75, Some(2))
        case (`lcns2`, Some(2)) => launchConfigRes( 76 to 100, None)
        case (`lcns3`, None)    => launchConfigRes(101 to 125, Some(2))
        case (`lcns3`, Some(2)) => launchConfigRes(126 to 150, None)
        case (`lcns4`, None)    => launchConfigRes(151 to 175, Some(2))
        case (`lcns4`, Some(2)) => launchConfigRes(176 to 200, None)
        case (`lcns5`, None)    => launchConfigRes(201 to 225, Some(2))
        case (`lcns5`, Some(2)) => launchConfigRes(226 to 250, None)
        case (`lcns6`, None)    => launchConfigRes(251 to 275, Some(2))
        case (`lcns6`, Some(2)) => launchConfigRes(276 to 300, None)
        case (`lcns7`, None)    => launchConfigRes(301 to 325, Some(2))
        case (`lcns7`, Some(2)) => launchConfigRes(326 to 350, None)
        case (`lcns8`, None)    => launchConfigRes(351 to 375, Some(2))
        case (`lcns8`, Some(2)) => launchConfigRes(376 to 400, None)
        case (`lcns9`, None)    => launchConfigRes(401 to 425, Some(2))
        case (`lcns9`, Some(2)) => launchConfigRes(426 to 450, None)
        case x                  => sys error s"Unknown input $x"
      }
    }

  private def asgRes(r: Range, nextToken: Option[Int]) =
    AsgRes(r.toVector.map(n => Asg(f"$n%03d", f"lc$n%03d")), nextToken)

  private def launchConfigRes(r: Range, nextToken: Option[Int]) =
    LaunchConfigRes(r.toVector.map(n => LaunchConfig(f"lc$n%03d")), nextToken)

  private def launchConfigNames(r: Range) = r.toVector.map(n => f"lc$n%03d")
  private val lcns1 = launchConfigNames(  1 to 50)
  private val lcns2 = launchConfigNames( 51 to 100)
  private val lcns3 = launchConfigNames(101 to 150)
  private val lcns4 = launchConfigNames(151 to 200)
  private val lcns5 = launchConfigNames(201 to 250)
  private val lcns6 = launchConfigNames(251 to 300)
  private val lcns7 = launchConfigNames(301 to 350)
  private val lcns8 = launchConfigNames(351 to 400)
  private val lcns9 = launchConfigNames(401 to 450)
}
