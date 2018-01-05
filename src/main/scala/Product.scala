package category

final case class Product[A, B](fst: A, snd: B)

object Product {
  type *[A, B] = Product[A, B]

  def projectFst[A]: Product[A, _] => A = _.fst
  def projectSnd[B]: Product[_, B] => B = _.snd
}

sealed trait Coproduct[A, B]
final case class CLeft[A, B](a: A) extends Coproduct[A, B]
final case class CRight[A, B](b: B) extends Coproduct[A, B]

object Coproduct {
  type +[A, B] = Coproduct[A, B]

  def injectLeft[A, B]: A => Coproduct[A, B] = CLeft(_)
  def injectRight[A, B]: B => Coproduct[A, B] = CRight(_)
}

