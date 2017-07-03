package purefunccats.db

import scala.util.{ Try, Success, Failure }

trait Database {
  def getUser(id: Int): Option[User] =
    if (id == 42) Some(User(id)) else None
}

object Database {
  def connect(dbOnline: Boolean): Try[Database] =
    if (dbOnline) Success(new Database {})
    else Failure(new RuntimeException("Database is offline"))  
}

object DatabaseApp {
  def main(args: Array[String]): Unit = {
    val connection = Database.connect(false)

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
