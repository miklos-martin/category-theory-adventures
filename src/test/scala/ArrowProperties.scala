package category

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait ArrowProperties {
  def arrowsEqual[A : Arbitrary, B](f: A => B, g: A => B) = forAll { a: A =>
    f(a) == g(a)
  }

  def arrowIsIdentity[A : Arbitrary](f: A => A) = arrowsEqual[A, A](f, identity)
}
