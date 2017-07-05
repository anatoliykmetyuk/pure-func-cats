package purefunccats.monads

import purefunccats.db.{ Database, User, Post }
import purefunccats.typeclass.ToEitherStr
import ToEitherStr.Ops

object Sequential {
  type EitherStr[A] = Either[String, A]

  def connection: EitherStr[Database] =
    Database.connect(true).toEitherStr

  def user(id: Int, db: Database): EitherStr[User] =
    db.getUser(id).toEitherStr

  def postWithTitleOfUser(title: String, user: User, db: Database): EitherStr[Post] =
    db.getPostsOf(user).find(_.title == title).toEitherStr


  def getPostOfWithTitle(userId: Int, postTitle: String): EitherStr[Post] =
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