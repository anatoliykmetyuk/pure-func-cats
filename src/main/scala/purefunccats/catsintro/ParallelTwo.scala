package purefunccats.catsintro

import purefunccats.db.{ Database, Post }
import purefunccats.monads.typeclass.{ LogEither, RightLog, LeftLog }

import cats.Cartesian
import cats.syntax.all._
import implicits._

import Sequential.{ connection, user, postWithTitleOfUser }

/**
  An adaptation of SequentialTwo example. It shows how
  the problem we encountered there has a solution
  in the form of the Cartesian type class from Cats.
  With Cartesian, we are able to execute two chunks of
  code independently and then merge their results into one structure.
 */
object ParallelTwo {
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
      c    <- connection
      pair <- Cartesian[LogEither[String, ?]].product(
        lookupPostForUser(userId1, postTitle1, c),
        lookupPostForUser(userId2, postTitle2, c))
    } yield pair

  def main(args: Array[String]): Unit = {
    val result = getPostOfTwoWithTitle(100, 42, "Foo", "Bar")
    
    result match {
      case RightLog(r, _) => println(s"Result: $r")
      case LeftLog (l, _) => println(s"Error: $l")
    }

    println("Log:\n" + result.log.mkString("\n"))
  }
}
