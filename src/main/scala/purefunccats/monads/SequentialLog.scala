package purefunccats.monads

import purefunccats.db.{ Database, User, Post }
import purefunccats.monads.typeclass._

import ToLogEitherStr.{Ops => Ops1}
import Monad.Ops


/**
 * This example shows how more side effects can be abstracted
 * into the effect types. In the Sequential example, we
 * only had errors abstracted - here, we have the logging
 * functionality added too.
 *    
 * See purefunccats.monads.typeclass package for the
 * classes involved.
 */
object SequentialLog {
  def connection: LogEither[String, Database] =
    Database.connect(true).toLogEitherStr

  def user(id: Int, db: Database): LogEither[String, User] =
    db.getUser(id).toLogEitherStr

  def postWithTitleOfUser(title: String, user: User, db: Database): LogEither[String, Post] =
    db.getPostsOf(user).find(_.title == title).toLogEitherStr

 /**
  * The code below is very similar to the imperative, side effecting
  * code. However, whatever the statements of this monadic flow
  * do, gets written to the effect type and does not side effect.
  */
  def getPostOfWithTitle(userId: Int, postTitle: String): LogEither[String, Post] =
    for {
      c <- connection // connection.flatMap { c => LogEither.tell[String](s"Connection established: $c") }
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
