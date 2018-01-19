package category

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait ArrowProperties {
  import Eq.syntax._

  implicit def arrowEq[A: Arbitrary, B : Eq]: Eq[A => B] = new Eq[A => B] {
    import Arbitrary.arbitrary
    def eqv(f1: A => B, f2: A => B): Boolean = {
      List.fill(50)(arbitrary[A].sample).flatten forall { a =>
        f1(a) === f2(a)
      }
    }
  }

  def arrowsEqual[A : Arbitrary, B : Eq](f: A => B, g: A => B) = forAll { a: A =>
    f(a) === g(a)
  }

  def arrowIsIdentity[A : Arbitrary : Eq](f: A => A) = arrowsEqual[A, A](f, identity)
}
