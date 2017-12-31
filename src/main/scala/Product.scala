package category

final case class Product[A, B](fst: A, snd: B)

object Product {
  type *[A, B] = Product[A, B]

  def projectFst[A]: Product[A, _] => A = _.fst
  def projectSnd[B]: Product[_, B] => B = _.snd
}

sealed trait Coproduct[A, B]
final case class One[A, B](a: A) extends Coproduct[A, B]
final case class Other[A, B](b: B) extends Coproduct[A, B]

object Coproduct {
  type +[A, B] = Coproduct[A, B]

  def injectFst[A, B]: A => Coproduct[A, B] = One(_)
  def injectSnd[A, B]: B => Coproduct[A, B] = Other(_)
}

