package purefunccats.monads.typeclass

trait Monad[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}

object Monad {
  def apply[F[_]](implicit e: Monad[F]) = e

  implicit class Ops[F[_]: Monad, A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] =
      Monad[F].flatMap(fa)(f)

    def map[B](f: A => B): F[B] =
      Monad[F].map(fa)(f)
  }

  implicit def monadLogEither[L]: Monad[({type f[a] = LogEither[L, a]})#f] = new Monad[({type f[a] = LogEither[L, a]})#f] {
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
  }
}