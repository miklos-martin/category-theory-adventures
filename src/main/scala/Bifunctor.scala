package category

trait Bifunctor[F[_, _]] {
  def bimap[A, T, B, U](f: A => T)(g: B => U): F[A, B] => F[T, U]
}

object Bifunctor {
  def apply[F[_, _]](implicit B: Bifunctor[F]) = B

  implicit def functorOnLeft[F[_, _] : Bifunctor, U] = new Functor[F[?, U]] {
    def fmap[A, B](f: A => B) = Bifunctor[F].bimap(f)(identity)
  }

  implicit def functorOnRight[F[_, _] : Bifunctor, T] = new Functor[F[T, ?]] {
    def fmap[A, B](f: A => B) = Bifunctor[F].bimap(identity: T => T)(f)
  }
}
