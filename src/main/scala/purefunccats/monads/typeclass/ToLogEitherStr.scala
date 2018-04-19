package purefunccats.monads.typeclass

import scala.util.{ Try, Success, Failure }


trait ToLogEitherStr[F[_]] {
  def toLogEitherStr[A](x: F[A]): LogEither[String, A]
}

object ToLogEitherStr {
  def apply[F[_]](implicit e: ToLogEitherStr[F]) = e

  implicit class Ops[F[_], A](f: F[A])(implicit e: ToLogEitherStr[F]) {
    def toLogEitherStr: LogEither[String, A] = e.toLogEitherStr(f)
  }

  implicit val tesOpt: ToLogEitherStr[Option] = new ToLogEitherStr[Option] {
    def toLogEitherStr[A](x: Option[A]) = x match {
      case Some(y) => LogEither.right(y)
      case None    => LogEither.left ("Empty Option")
    }
  }

  implicit val tesTry: ToLogEitherStr[Try] = new ToLogEitherStr[Try] {
    def toLogEitherStr[A](x: Try[A]): LogEither[String, A] = x match {
      case Success(y) => LogEither.right(y)
      case Failure(e) => LogEither.left (s"Error occurred: ${e.getMessage}")
    }
  }
}
