package category

import org.scalacheck.{Arbitrary, Gen, Properties}
import Product._
import Coproduct._

trait ProductGenerators {
  import Arbitrary.arbitrary

  implicit def genProduct[A : Arbitrary, B : Arbitrary]: Gen[A * B] = for {
    a <- arbitrary[A]
    b <- arbitrary[B]
  } yield Product(a, b)
  implicit def arbitraryProduct[A : Arbitrary, B : Arbitrary] = Arbitrary(genProduct[A, B])

  def one[A : Arbitrary, B]: Gen[One[A, B]] = for (a <- arbitrary[A]) yield One(a)
  def other[A, B : Arbitrary]: Gen[Other[A, B]] = for (b <- arbitrary[B]) yield Other(b)
  implicit def arbitrarySum[A : Arbitrary, B : Arbitrary]: Arbitrary[A + B] = Arbitrary(Gen.oneOf(one[A, B], other[A, B]))
}

object TypelevelAlgebra extends Properties("TypelevelAlgebra") with IsomorphicProperties with ProductGenerators {
  // a * (b + c) = a*b + a*c
  implicit def prod_vs_sum[A, B, C] = new Isomorphism[A * (B + C), (A * B) + (A * C)] {
    def a2b = {
      case Product(a, One(b)) => One(Product(a, b))
      case Product(a, Other(c)) => Other(Product(a, c))
    }

    def b2a = {
      case One(Product(a, b)) => Product(a, One(b))
      case Other(Product(a, c)) => Product(a, Other(c))
    }
  }

  proveTypesAreIsoMorphic[String * (Boolean + Int), (String * Boolean) + (String * Int)]

  // a * 1 = a
  implicit def unitOfProduct[A] = new Isomorphism[A * Unit, A] {
    def a2b = projectFst
    def b2a = Product(_, ())
  }

  proveTypesAreIsoMorphic[String * Unit, String]

  // a + 0 = a
  // implicit def unitOfSum[A] = new Isomorphism[A + Nothing, A] {
  //   def a2b = {
  //     case One(a) => a
  //     case _ => throw new IllegalStateException("The world has just blown up, there is an instance of Nothing")
  //   }
  //   def b2a = One(_)
  // }

  // proveTypesAreIsoMorphic[String + Nothing, String]
}
