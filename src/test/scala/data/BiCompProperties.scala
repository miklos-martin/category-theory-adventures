package category
package data

import org.scalacheck.{Arbitrary, Properties}
import Arbitrary.arbitrary

trait BiCopGen {
  implicit def genBiComp[BF[_, _], F[_], G[_], A: Arbitrary, B: Arbitrary]
    (implicit a3: Arbitrary[BF[F[A], G[B]]]): Arbitrary[BiComp[BF, F, G, A, B]] =
    Arbitrary(for(x <- arbitrary[BF[F[A], G[B]]]) yield BiComp(x))
}

object BiCompExample extends Properties("BiComp example with tuple[option, list]") with BifunctorLaws with BiCopGen {
  import instances.tuple2._
  import instances.option._
  import instances.list._

  proveBifunctorLaws[BiComp[Tuple2, Option, List, ?, ?]]
}

object BiCompAsOption extends Properties("BiComp example building ADTs (Option) from primitives") with BifunctorLaws with BiCopGen with ConstGen {
  import instances.either._

  // I thought these could be handled by the compiler
  implicit def genBC[A: Arbitrary, B: Arbitrary]: Arbitrary[BiComp[Either, Const[Unit, ?], Id, A, B]] =
    genBiComp[Either, Const[Unit, ?], Id, A, B]
  implicit val bf: Bifunctor[BiComp[Either, Const[Unit, ?], Id, ?, ?]] =
    BiComp.bifunctor[Either, Const[Unit, ?], Id]

  proveBifunctorLaws[BiComp[Either, Const[Unit, ?], Id, ?, ?]]
}
