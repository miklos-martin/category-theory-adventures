package category
package data

case class Const[C, A](c: C)

object Const {
  implicit def functor[C] = new Functor[Const[C, ?]] {
    def fmap[A, B] = _ => {
      case Const(c) => Const(c)
    }
  }

  implicit val bifunctor = new Bifunctor[Const] {
    override def bimap[A, T, B, U] = f => _ => {
      case Const(c) => Const(f(c))
    }
  }
}
