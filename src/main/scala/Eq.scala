package category

trait Eq[A] {
  def eqv(a1: A, a2: A): Boolean
}

object Eq {
  def apply[A](implicit eq: Eq[A]) = eq

  implicit def universal[A] = new Eq[A] {
    def eqv(a1: A, a2: A) = a1 == a2
  }

  object syntax {
    implicit class EqOps[A : Eq](a: A) {
      def ===(b: A) = Eq[A].eqv(a, b)
      def =/=(b: A) = !(===(b))
    }
  }
}
