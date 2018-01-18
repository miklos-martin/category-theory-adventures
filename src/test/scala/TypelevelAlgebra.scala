package category

import org.scalacheck.{Arbitrary, Gen, Properties}
import Product._
import Coproduct._

trait ProductGenerators {
  // Numbers denote how many instances a given type has
  abstract final class `0`
  type `1` = Unit
  type `2` = Boolean

  implicit val arbitraryZero: Arbitrary[`0`] = Arbitrary(Gen.fail)
}

object TypelevelAlgebra extends Properties("TypelevelAlgebra") with IsomorphicProperties with ProductGenerators {

  // a * b = b * a
  implicit def commutativityForProduct[A, B]  = new Isomorphism[A * B, B * A] {
    def a2b = Product.swap
    def b2a = Product.swap
  }
  proveTypesAreIsoMorphic[String * Int, Int * String]

  // a * 1 = a
  implicit def unitOfProduct[A] = new Isomorphism[A * `1`, A] {
    def a2b = projectFst
    def b2a = (_, ())
  }
  proveTypesAreIsoMorphic[String * `1`, String]

  // a * (b * c) = (a * b) * c
  implicit def associavityForProduct[A, B, C] = new Isomorphism[A * (B * C), (A * B) * C] {
    def a2b = { case (a, (b, c)) => ((a, b), c) }
    def b2a = { case ((a, b), c) => (a, (b, c)) }
  }
  proveTypesAreIsoMorphic[String * (Int * Boolean), (String * Int) * Boolean]


  // a + b = b + a
  implicit def commutativityForSum[A, B] = new Isomorphism[A + B, B + A] {
    def a2b = Coproduct.swap
    def b2a = Coproduct.swap
  }
  proveTypesAreIsoMorphic[String + Int, Int + String]

  // a + 0 = a
  implicit def unitOfSum[A] = new Isomorphism[A + `0`, A] {
    def a2b = {
      case Left(a) => a
      case _ => throw new IllegalStateException("The world has just blown up, there is an instance of `0`")
    }
    def b2a = injectLeft
  }
  proveTypesAreIsoMorphic[String + `0`, String]

  // a + (b + c) = (a + b) + c
  implicit def associavityForSum[A, B, C] = new Isomorphism[A + (B + C), (A + B) + C] {
    def a2b = {
      case Left(a) => Left(Left(a))
      case Right(Left(b)) => Left(Right(b))
      case Right(Right(c)) => Right(c)
    }

    def b2a = {
      case Left(Left(a)) => Left(a)
      case Left(Right(b)) => Right(Left(b))
      case Right(c) => Right(Right(c))
    }
  }
  proveTypesAreIsoMorphic[String + (Int + Boolean), (String + Int) + Boolean]

  // a + a = 2 * a
  implicit def `a+a=2*a`[A] = new Isomorphism[A + A, `2` * A] {
    def a2b = {
      case Left(a) => (true, a)
      case Right(a) => (false, a)
    }

    def b2a = {
      case (true, a) => injectLeft(a)
      case (false, a) => injectRight(a)
    }
  }
  proveTypesAreIsoMorphic[String + String, `2` * String]


  // a * (b + c) = a * b + a * c
  implicit def distributivity[A, B, C] = new Isomorphism[A * (B + C), (A * B) + (A * C)] {
    def a2b = {
      case (a, Left(b)) => injectLeft((a, b))
      case (a, Right(c)) => injectRight((a, c))
    }

    def b2a = {
      case Left((a, b)) => (a, injectLeft(b))
      case Right((a, c)) => (a, injectRight(c))
    }
  }
  proveTypesAreIsoMorphic[String * (Boolean + Int), (String * Boolean) + (String * Int)]
}
