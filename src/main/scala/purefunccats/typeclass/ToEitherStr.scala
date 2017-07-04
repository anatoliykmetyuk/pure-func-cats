package purefunccats.typeclass

import scala.util.{ Try, Success, Failure }

trait ToEitherStr[F[_]] {
  def toEitherStr[A](x: F[A]): Either[String, A]
}

object ToEitherStr {
  def apply[F[_]](implicit e: ToEitherStr[F]) = e

  implicit class Ops[F[_], A](f: F[A])(implicit e: ToEitherStr[F]) {
    def toEitherStr: Either[String, A] = e.toEitherStr(f)
  }

  implicit val tesOpt: ToEitherStr[Option] = new ToEitherStr[Option] {
    def toEitherStr[A](x: Option[A]) = x match {
      case Some(y) => Right(y)
      case None    => Left("Empty Option")
    }
  }

  implicit val tesTry: ToEitherStr[Try] = new ToEitherStr[Try] {
    def toEitherStr[A](x: Try[A]): Either[String, A] = x match {
      case Success(y) => Right(y)
      case Failure(e) => Left(s"Error occurred: ${e.getMessage}")
    }
  }
}

object ToEitherStrApp extends App {
  import ToEitherStr.Ops

  val some: Option[Int] = Some(1)
  val none: Option[Int] = None
  val success: Try[Int] = Success(1)
  val failure: Try[Int] = Failure(new RuntimeException("Test Exception"))

  def mkEitherString[F[_]: ToEitherStr, A](l: List[F[A]]): String =
    l.map(_.toEitherStr).mkString("\n")
  
  val str =
    mkEitherString(List(some, none)) + "\n" +
    mkEitherString(List(success, failure))

  println(str)
}
