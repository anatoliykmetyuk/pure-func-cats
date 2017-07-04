package purefunccats.monads

import purefunccats.db.{ Database, User, Post }
import purefunccats.monads.typeclass._

import ToLogEitherStr.{Ops => Ops1}
import Monad.Ops


object SequentialLog {
  def connection: LogEither[String, Database] =
    Database.connect(true).toLogEitherStr

  def user(id: Int, db: Database): LogEither[String, User] =
    db.getUser(id).toLogEitherStr

  def postWithTitleOfUser(title: String, user: User, db: Database): LogEither[String, Post] =
    db.getPostsOf(user.id).find(_.title == title).toLogEitherStr


  def getPostOfWithTitle(userId: Int, postTitle: String): LogEither[String, Post] =
    for {
      c <- connection
      _ <- LogEither.tell[String](s"Connection established: $c")

      u <- user(userId, c)
      _ <- LogEither.tell[String](s"Obtained user: $u")

      p <- postWithTitleOfUser(postTitle, u, c)
      _ <- LogEither.tell[String](s"Post found: $p")
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
