package purefunccats.catsintro

import cats.Monad

import purefunccats.monads.typeclass.{ LogEither, RightLog, LeftLog }


object implicits {

  /**
    Kind Projector plugin is used below to partially apply a type.
    This avoids the hassle with the type lambda we saw in
    `purefunccats.monads.typeclass.Monad`.
  */
  implicit def logEitherMonad[L]: Monad[LogEither[L, ?]] = new Monad[LogEither[L, ?]] {
    def flatMap[A, B](fa: LogEither[L, A])(f: A => LogEither[L, B]): LogEither[L, B] =
      fa match {
        case LeftLog (left , log) => LeftLog(left, log)
        case RightLog(right, log) =>
          val bLog = f(right)
          bLog match {
            case LeftLog (left , log2) => LeftLog (left , log ++ log2)
            case RightLog(right, log2) => RightLog(right, log ++ log2)
          }
      }
    
    def pure[A](a: A): LogEither[L, A] = RightLog(a, Nil)
  
    def tailRecM[A, B](a: A)(f: A => LogEither[L, Either[A, B]]): LogEither[L, B] = ???
    
    override def product[A, B](fa: LogEither[L, A], fb: LogEither[L, B]): LogEither[L, (A, B)] =
      (fa, fb) match {
        case (RightLog(a, l1), RightLog(b, l2)) => RightLog((a, b), l1 ++ l2    )
        case (RightLog(_, l1), LeftLog (e, l2)) => LeftLog (e     , l1 ++ l2    )
        case (LeftLog (e, l1), _              ) => LeftLog (e     , l1 ++ fb.log)
      }
  }
}
