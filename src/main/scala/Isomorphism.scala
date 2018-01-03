package category

trait Isomorphism[A, B] {
  def a2b: A => B
  def b2a: B => A
}

object Isomorphism {
  type <=>[A, B] = Isomorphism[A, B]

  object syntax {
    implicit class IsoOps[A](val a: A) extends AnyVal {
      def as[B](implicit iso: A <=> B) = iso.a2b(a)
    }
  }

  def apply[A, B](implicit ev: A <=> B) = ev
  def apply[A] = self[A]

  implicit def self[A] = new Isomorphism[A, A] {
    def a2b = identity
    def b2a = identity
  }

  implicit def reverse[A, B](implicit iso: A <=> B) = new Isomorphism[B, A] {
    def a2b = iso.b2a
    def b2a = iso.a2b
  }
}

