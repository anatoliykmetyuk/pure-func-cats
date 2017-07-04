package purefunccats.typeclass

import scala.util.{ Try, Success, Failure }

object RichWrappers {

  trait ToEitherStr[A] {
    def toEitherStr: Either[String, A]
  }

  // Option to Either Rich Wrapper
  implicit class ToEitherStrOpt[A](x: Option[A]) extends ToEitherStr[A] {
    def toEitherStr: Either[String, A] = x match {
      case Some(y) => Right(y)
      case None    => Left("Empty Option")
    }
  }

  // implicit def toEitherStrOpt[A](x: Option[A]) =
  //   new ToEitherStrOpt(x)

  // Try to Either Rich Wrapper
  implicit class ToEitherStrTry[A](x: Try[A]) extends ToEitherStr[A] {
    def toEitherStr: Either[String, A] = x match {
      case Success(y) => Right(y)
      case Failure(e) => Left(s"Error occurred: ${e.getMessage}")
    }
  }

  val some: Option[Int] = Some(1)
  val none: Option[Int] = None
  val success: Try[Int] = Success(1)
  val failure: Try[Int] = Failure(new RuntimeException("Test Exception"))
  
  def _main(args: Array[String]): Unit = {
    val str =
      (List(some, none).map(_.toEitherStr) ++
       List(success, failure).map(_.toEitherStr)).mkString("\n")

    println(str)
  }

  def main(args: Array[String]): Unit = {
    def mkEitherString[F[_], A](l: List[F[A]])(implicit e: F[A] => ToEitherStr[A]): String =
      l.map(_.toEitherStr).mkString("\n")
    
    val str =
      mkEitherString(List(some, none)) + "\n" +
      mkEitherString(List(success, failure))

    println(str)
  }
}
