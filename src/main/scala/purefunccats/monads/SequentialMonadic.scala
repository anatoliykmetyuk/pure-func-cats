package purefunccats.monads

import purefunccats.db.Post

import Sequential._

object SequentialMonadic {
  def getPostOfWithTitleMonadic(userId: Int, postTitle: String): Either[String, Post] =
    connection.flatMap { c =>  // Get connection
      user(userId, c).flatMap { u =>  // Get User
        postWithTitleOfUser(postTitle, u, c)  // Get posts of the user
      }
    }

  def getPostOfWithTitleFor(userId: Int, postTitle: String): Either[String, Post] =
    for {
      c <- connection
      u <- user(userId, c)
      p <- postWithTitleOfUser(postTitle, u, c)
    } yield p


  def main(args: Array[String]): Unit = {
    println(getPostOfWithTitleFor(42, "Feoo"))
  }
}