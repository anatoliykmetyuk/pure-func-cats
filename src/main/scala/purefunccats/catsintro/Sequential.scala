package purefunccats.catsintro

import purefunccats.db.{ Database, User, Post }
import purefunccats.monads.typeclass.{ LogEither, RightLog, LeftLog }
import purefunccats.monads.typeclass.ToLogEitherStr.Ops

import cats.syntax.flatMap._, cats.syntax.functor._
import implicits._


object Sequential {
  def connection: LogEither[String, Database] =
    for {
      db <- Database.connect(true).toLogEitherStr
      _  <- LogEither.tell[String](s"Connection established: $db")
    } yield db

  def user(id: Int, db: Database): LogEither[String, User] =
    for {
      u <- db.getUser(id).toLogEitherStr
      _ <- LogEither.tell[String](s"Obtained user: $u")
    } yield u

  def postWithTitleOfUser(title: String, user: User, db: Database): LogEither[String, Post] =
    for {
      p <- db.getPostsOf(user).find(_.title == title).toLogEitherStr
      _ <- LogEither.tell[String](s"Post found for $user: $p")
    } yield p

  def getPostOfWithTitle(userId: Int, postTitle: String): LogEither[String, Post] =
    for {
      c <- connection
      u <- user(userId, c)
      p <- postWithTitleOfUser(postTitle, u, c)
    } yield p

  def main(args: Array[String]): Unit = {
    val result = getPostOfWithTitle(42, "Foo")
    
    result match {
      case RightLog(r, _) => println(s"Result: $r")
      case LeftLog (l, _) => println(s"Error: $l")
    }

    println("Log:\n" + result.log.mkString("\n"))
  }
}
