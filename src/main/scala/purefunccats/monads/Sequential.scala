package purefunccats.monads

import purefunccats.db.{ Database, User, Post }
import purefunccats.typeclass.ToEitherStr
import ToEitherStr.Ops

/**
 * The algorithm of this application is as follows:
 *
 * 1. Establish a database connection
 * 2. Get the user with a given ID
 * 3. Get all their posts and filter the ones with a given title
 * 
 * Three methods below represent the three
 * steps of this algorithm.
 * These methods are independent pieces of logic.
 * They are hence abstracted from the main logic of the
 * application so that the program is modular.
 * 
 * The thing to notice about them is what goes in and
 * what goes out.
 * Inputs are raw values (A), but the outputs are F[A] -
 * the effect type.
 * 
 * The inputs are this way (A and not F[A]) because
 * the details of the analysis of F[A] are not part of the
 * logic we want to encapsulate. We want to, say, describe
 * only how to get the user with a given id from a database -
 * not what to do if we failed to establish the connection in
 * the first place. If you failed to establish the DB connection -
 * don't call the user() method in first place.
 *
 * The outputs are of the type F[A] and not A, because
 * the imperative versions of the functions would have
 * side effected (null returned instead of the user,
 * exception thrown during the database
 * connection establishment). We have purified them
 * via effect types, thus the result value is wrapped in the
 * effect type we used.
 */
object Sequential {
  // EitherStr[A] is introduced so that the
  // effect type we are working under is of the
  // form F[A]. Either[String, A] is of the form F[B, A].
  type EitherStr[A] = Either[String, A]

  def connection: EitherStr[Database] =
    Database.connect(true).toEitherStr

  def user(id: Int, db: Database): EitherStr[User] =
    db.getUser(id).toEitherStr

  def postWithTitleOfUser(title: String, user: User, db: Database): EitherStr[Post] =
    db.getPostsOf(user).find(_.title == title).toEitherStr

  /**
   * This method combines the three methods above by calling them
   * in sequence. Notice how hard it is to combine effect computations
   * (functions that accept raw values and return effect types - A => F[A]).
   */
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