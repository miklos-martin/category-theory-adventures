package category

object Product {
  type *[A, B] = (A, B)

  def projectFst[A]: A * _ => A = _._1
  def projectSnd[B]: _ * B => B = _._2

  def swap[A, B]: A * B => B * A = {
    case (a, b) => (b, a)
  }
}

object Coproduct {
  type +[A, B] = Either[A, B]

  def injectLeft[A, B]: A => A + B = Left(_)
  def injectRight[A, B]: B => A + B = Right(_)

  def swap[A, B]: A + B => B + A = {
    case Left(a) => Right(a)
    case Right(b) => Left(b)
  }
}
