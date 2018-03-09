package category

trait Bifunctor[F[_, _]] {
  def bimap[A, T, B, U]: (A => T) => (B => U) => F[A, B] => F[T, U] =
    f => g => first(f) compose second(g)

  def first[A, B, C]: (A => C) => F[A, B] => F[C, B] =
    bimap(_)(identity)

  def second[A, B, D]: (B => D) => F[A, B] => F[A, D] =
    bimap(identity)
}

object Bifunctor {
  def apply[F[_, _]](implicit B: Bifunctor[F]) = B

  implicit def functorOnLeft[F[_, _] : Bifunctor, U] = new Functor[F[?, U]] {
    def fmap[A, B] = Bifunctor[F].first
  }

  implicit def functorOnRight[F[_, _] : Bifunctor, T] = new Functor[F[T, ?]] {
    def fmap[A, B] = Bifunctor[F].second
  }

  implicit def compose[F[_, _]: Bifunctor, G[_, _]: Bifunctor] = new Bifunctor[λ[(A, B) => F[G[A, B], G[A, B]]]] {
    override def bimap[A, T, B, U] = f => g => {
      val inner = Bifunctor[G].bimap(f)(g)
      Bifunctor[F].bimap(inner)(inner)
    }
  }
}
