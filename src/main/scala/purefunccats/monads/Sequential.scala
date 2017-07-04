package purefunccats.monads

import purefunccats.db.{ Database, User, Post }
import purefunccats.typeclass.ToEitherStr
import ToEitherStr.Ops

object Sequential {
  def connection: Either[String, Database] =
    Database.connect(true).toEitherStr

  def user(id: Int, db: Database): Either[String, User] =
    db.getUser(id).toEitherStr

  def postWithTitleOfUser(title: String, user: User, db: Database): Either[String, Post] =
    db.getPostsOf(user.id).find(_.title == title).toEitherStr


  def getPostOfWithTitle(userId: Int, postTitle: String): Either[String, Post] =
    // Get connection
    connection match {
      case Right(c) =>
        // Get User
        user(userId, c) match {
          case Right(u) =>
            // Get posts of the user
            postWithTitleOfUser(postTitle, u, c)
          
          case Left(msg) => Left(msg)
        }

      case Left(msg) => Left(msg)
    }

  def main(args: Array[String]): Unit =
    println(getPostOfWithTitle(42, "Foo"))
}