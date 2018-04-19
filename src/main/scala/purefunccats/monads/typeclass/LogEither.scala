package purefunccats.monads.typeclass

/**
 Note how these traits have no methods associated with them.
 We are following the philosophy that all the methods should
 be stored separately from the class definitions themselves.

 The advantage here is that now the behavior of the class
 is completely defined by the type class instances implemented for it.

 The advantage of the type classes over methods defined in classes
 is that a 3rd party library may define its own type classes
 and inject them in a data type. You can't do that with the
 native methods.

 We want to treat all the behaviors the data has the same way.
 Consider for example a `flatMap` method defined as a class member.
 This way, it is impossible to override it by implementing a Monad
 type class for it. The compiler knows that the class has `flatMap`
 natively and will not even try to look up the Monad type class
 via implicits mechanism. For example, try to implement a Monad for
 Either and call flatMap on some Either.

 With the type classes, you can replace the type classes you don't like
 with alternative implementations of your own, as well as to define
 your own new type classes.

 Bottom line: we want all the behaviors on the data to be treated the same.
 We have a choice between the native methods and the type class technique.
 Since type class technique is superior (allows for 3rd parties to extend
 our libraries), we select it.
 */
trait LogEither[+L, +R] { val log: List[String] }
case class LeftLog [L](left : L, log: List[String]) extends LogEither[L, Nothing]
case class RightLog[R](right: R, log: List[String]) extends LogEither[Nothing, R]

object LogEither {
  def tell[L](msg: String): LogEither[L, Unit] = RightLog((), List(msg))
  def right[R](r: R): LogEither[Nothing, R] = RightLog(r, Nil)
  def left [L](l: L): LogEither[L, Nothing] = LeftLog (l, Nil)
}