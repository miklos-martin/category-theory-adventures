package category
package data

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Arbitrary.arbitrary

trait ConstGen {
  implicit def gen[C: Arbitrary, A]: Arbitrary[Const[C, A]] =
    Arbitrary(for (c <- arbitrary[C]) yield Const(c))
}

object ConstAsFunctor extends Properties("Const functor") with FunctorLaws with ConstGen {
  proveFunctorLaws[Const[Int, ?]]
}

object ConstAsBifunctor extends Properties("Const bifunctor") with BifunctorLaws with ConstGen {
  proveBifunctorLaws[Const]
}
