package category
package instances

trait EitherInstances {
  implicit val eitherBifunctor = new Bifunctor[Either] {
    override def bimap[A, T, B, U] = f => g => {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }
}
