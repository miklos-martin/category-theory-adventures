package category

import org.scalacheck.{Arbitrary, Gen, Properties}
import Product._
import Coproduct._

trait ProductGenerators {
  import Arbitrary.arbitrary

  // Numbers denote how many instances a given type has
  abstract final class `0`
  type `1` = Unit
  type `2` = Boolean

  implicit val arbitraryZero: Arbitrary[`0`] = Arbitrary(Gen.fail)

  implicit def genProduct[A : Arbitrary, B : Arbitrary]: Gen[A * B] = for {
    a <- arbitrary[A]
    b <- arbitrary[B]
  } yield Product(a, b)
  implicit def arbitraryProduct[A : Arbitrary, B : Arbitrary] = Arbitrary(genProduct[A, B])

  def left[A : Arbitrary, B]: Gen[Left[A, B]] = for (a <- arbitrary[A]) yield Left(a)
  def right[A, B : Arbitrary]: Gen[Right[A, B]] = for (b <- arbitrary[B]) yield Right(b)
  implicit def arbitrarySum[A : Arbitrary, B : Arbitrary]: Arbitrary[A + B] = Arbitrary(Gen.oneOf(left[A, B], right[A, B]))
}

object TypelevelAlgebra extends Properties("TypelevelAlgebra") with IsomorphicProperties with ProductGenerators {
  // a * (b + c) = a * b + a * c
  implicit def prod_vs_sum[A, B, C] = new Isomorphism[A * (B + C), (A * B) + (A * C)] {
    def a2b = {
      case Product(a, Left(b)) => injectLeft(Product(a, b))
      case Product(a, Right(c)) => injectRight(Product(a, c))
    }

    def b2a = {
      case Left(Product(a, b)) => Product(a, injectLeft(b))
      case Right(Product(a, c)) => Product(a, injectRight(c))
    }
  }

  proveTypesAreIsoMorphic[String * (Boolean + Int), (String * Boolean) + (String * Int)]

  // a * 1 = a
  implicit def unitOfProduct[A] = new Isomorphism[A * `1`, A] {
    def a2b = projectFst
    def b2a = Product(_, ())
  }

  proveTypesAreIsoMorphic[String * `1`, String]

  // a + 0 = a
  implicit def unitOfSum[A] = new Isomorphism[A + `0`, A] {
    def a2b = {
      case Left(a) => a
      case _ => throw new IllegalStateException("The world has just blown up, there is an instance of `0`")
    }
    def b2a = injectLeft
  }

  proveTypesAreIsoMorphic[String + `0`, String]

  // a + a = 2 * a
  implicit def `a+a=2*a`[A] = new Isomorphism[A + A, `2` * A] {
    def a2b = {
      case Left(a) => Product(true, a)
      case Right(a) => Product(false, a)
    }

    def b2a = {
      case Product(true, a) => injectLeft(a)
      case Product(false, a) => injectRight(a)
    }
  }

  proveTypesAreIsoMorphic[String + String, `2` * String]
}
