package category

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait ArrowProperties {
  import Eq.syntax._

  def arrowsEqual[A : Arbitrary, B : Eq](f: A => B, g: A => B) = forAll { a: A =>
    f(a) === g(a)
  }

  def arrowIsIdentity[A : Arbitrary](f: A => A) = arrowsEqual[A, A](f, identity)
}
