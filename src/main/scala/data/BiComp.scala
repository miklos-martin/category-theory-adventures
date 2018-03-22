package category
package data

case class BiComp[BF[_, _], F[_], G[_], A, B](x: BF[F[A], G[B]])

object BiComp {
  implicit def bifunctor[BF[_, _]: Bifunctor, F[_]: Functor, G[_]: Functor] = new Bifunctor[BiComp[BF, F, G, ?, ?]] {
    override def bimap[A, T, B, U] = f => g => {
      case BiComp(x) => BiComp(Bifunctor[BF].bimap(Functor[F].fmap(f))(Functor[G].fmap(g))(x))
    }
  }
}
