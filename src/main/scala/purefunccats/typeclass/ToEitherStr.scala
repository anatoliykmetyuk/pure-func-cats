package purefunccats.typeclass

import scala.util.{ Try, Success, Failure }

/**
  The type class pattern consists of the following parts:

  1. The trait with a type parameter. The type parameter
     is used by the implementations to specify which type
     this implementation of the type class is for.
  2. The body of the trait, containing all the abstract methods
     that are meant to be supported.
  3. The companion object with:
  3.1. The `apply` method with a single type argument - to be
       able to fetch the type class for any type from the
       implicit scope as follows: `ToEitherStr[Try]`.
       The alternative is the `implicitly` core Scala method:
       `implicitly[ToEitherStr[Try]]` - which is a bit longer.
  3.2. Rich Wrapper to inject the methods into any type which
       has the type class to it.
  3.3. Default implementations. Whenever the compiler needs an
       implicit, it looks up the companions of all the types
       involved. For instance, `implicitly[Foo[Bar]]` will look
       for the implicits in the companions of both `Foo` and `Bar`.
       This way, you do not need to import the type classes defined
       in the companions.

  The pattern may vary from library to library (e.g.
  Cats stores its Rich Wrappers outside the companions, and
  there is a Simulacrum library to generate most of the code
  below via a simple annotation), but the essence
  stays the same.
 */
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
