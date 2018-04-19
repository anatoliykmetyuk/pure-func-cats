package purefunccats.db

import scala.util.{ Try, Success, Failure }

/**
 * A pure database. Functions no longer have side effects.
 */
trait Database {
  /**
   * The effect of optionality of the result
   * (sometimes the result is there, sometimes it is not)
   * is reflected by the return type.
   *
   * The Option structure contains the information about
   * whether optionality effect took place.
   * If it did not took place, this structure is Some,
   * and it has the result. Otherwise - it is None.
   */
  def getUser(id: Int): Option[User] =
    if (id == 42 || id == 100) Some(User(id)) else None

  def getPostsOf(u: User): List[Post] =
    if (u.id == 42) List(Post("Foo"), Post("Bar")) else Nil
}

object Database {
  /**
   * The side effect (exception) is replaced with a structure
   * that has the information about the exception to be thrown,
   * but does not actually throw it or does any other side effect.
   */
  def connect(dbOnline: Boolean): Try[Database] =
    if (dbOnline) Success(new Database {})
    else Failure(new RuntimeException("Database is offline"))  
}

object DatabaseApp {
  def main(args: Array[String]): Unit = {
    val connection = Database.connect(false)

    // Safety of the code comes at a price:
    // analysis of the effect types is hard to read.
    connection match {
      case Success(c) =>
        val user = c.getUser(10)
        user match {
          case Some(u) => println(u.id)
          case None    => println("User not found")
        }

      case Failure(e) =>
        println(s"Error establishing connection: ${e.getMessage}.")
    }
  }
}
