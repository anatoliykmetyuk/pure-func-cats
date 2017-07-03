package purefunccats.db

trait EffectfulDatabase {
  def getUser(id: Int): User =
    if (id == 42) User(id) else null
}

object EffectfulDatabase {
  def connect(dbOnline: Boolean): EffectfulDatabase =
    if (dbOnline) new EffectfulDatabase {}
    else throw new RuntimeException("Database is offline")
}

object EffectfulDatabaseApp {
  def main(args: Array[String]): Unit = {
    val connection = EffectfulDatabase.connect(false)
    val user = connection.getUser(10)
    println(user.id)
  }
}