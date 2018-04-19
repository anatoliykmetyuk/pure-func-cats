package purefunccats.catsintro

import purefunccats.db.{ Database, User, Post }
import purefunccats.monads.typeclass.{ LogEither, RightLog, LeftLog }
import purefunccats.monads.typeclass.ToLogEitherStr.Ops

import cats.syntax.flatMap._, cats.syntax.functor._
import implicits._

import Sequential.{ connection, user, postWithTitleOfUser }

/**
  Notice how in this example,
  the program executes till the first error.
  Even if the chunk of code after the error
  does not depend on the chunk of code the error happened in.
  The problem here is that not all the errors are written into the
  log since the program does not try to execute past the first
  error.
*/
object SequentialTwo {
  def lookupPostForUser(uid: Int, postTitle: String, c: Database): LogEither[String, Post] =
    for {
      u <- user(uid, c)
      p <- postWithTitleOfUser(postTitle, u, c)
    } yield p

  def getPostOfTwoWithTitle(
    userId1: Int, userId2: Int,
    postTitle1: String, postTitle2: String
  ): LogEither[String, (Post, Post)] =
    for {
      c <- connection
      p1 <- lookupPostForUser(userId1, postTitle1, c)  // lookupPostForUser(userId1, postTitle1, c).flatMap { _ => lookupPostForUser(userId2, postTitle2, c) }
      p2 <- lookupPostForUser(userId2, postTitle2, c)
    } yield (p1, p2)

  def main(args: Array[String]): Unit = {
    val result = getPostOfTwoWithTitle(100, 42, "Foo", "Bar")
    
    result match {
      case RightLog(r, _) => println(s"Result: $r")
      case LeftLog (l, _) => println(s"Error: $l")
    }

    println("Log:\n" + result.log.mkString("\n"))
  }
}
