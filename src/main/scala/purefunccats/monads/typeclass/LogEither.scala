package purefunccats.monads.typeclass

trait LogEither[+L, +R] { val log: List[String] }
case class LeftLog [L](left : L, log: List[String]) extends LogEither[L, Nothing]
case class RightLog[R](right: R, log: List[String]) extends LogEither[Nothing, R]

object LogEither {
  def tell[L](msg: String): LogEither[L, Unit] = RightLog((), List(msg))
  def right[R](r: R): LogEither[Nothing, R] = RightLog(r, Nil)
  def left [L](l: L): LogEither[L, Nothing] = LeftLog (l, Nil)
}