package category

trait Functor[F[_]] {
  def fmap[A, B]: (A => B) => F[A] => F[B]
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]) = F

  implicit def composition[F[_] : Functor, G[_] : Functor] = new Functor[λ[A => F[G[A]]]] {
    def fmap[A, B]: (A => B) => F[G[A]] => F[G[B]] = Functor[F].fmap compose Functor[G].fmap
  }
}
