package flist

import scala.concurrent.{ ExecutionContext => EC, Future }

final case class FutureOption[+A](value: Future[Option[A]]) extends AnyVal {
  // Functor-based
  def            map[B](f:        A  =>               B  )(implicit ec: EC): FutureOption[B] = FutureOption(value map (_ map f))
  def     subflatMap[B](f:        A  =>        Option[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value map (_ flatMap f))
  def      transform[B](f: Option[A] =>        Option[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value map f)
  def   subcoflatMap[B](f: Option[A] =>               B  )(implicit ec: EC): FutureOption[B] = FutureOption(value map (o => Some(f(o))))

  // Monad-based
  def        flatMap[B](f:        A  =>  FutureOption[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap { case None => Future successful None ; case Some(a) => f(a).value })
  def  flatTransform[B](f: Option[A] =>  FutureOption[B] )(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap (o => f(o).value))

  // Monad-based, accepting the underlying Repr
  def       flatMapF[B](f:        A  => Future[Option[B]])(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap { case None => Future successful None ; case Some(a) => f(a) })
  def flatTransformF[B](f: Option[A] => Future[Option[B]])(implicit ec: EC): FutureOption[B] = FutureOption(value flatMap f)
}

object FutureOption {
  def empty[A]             : FutureOption[A] = FutureOption(Future successful None)
  def of[A](x: A)          : FutureOption[A] = FutureOption(Future successful Some(x))
  def opt[A](x: Option[A]) : FutureOption[A] = FutureOption(Future successful x)
}
