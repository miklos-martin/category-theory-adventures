package category

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import scala.reflect.runtime.universe._

object EqProperties extends Properties("Eq") {
  import Eq.syntax._

  def eqForType[A : Arbitrary : Eq : TypeTag] = {
    property(s"=== true for same instance of ${typeTag[A].tpe}") = forAll { a: A =>
      a === a
    }
  }

  eqForType[String]
  eqForType[Int]
  eqForType[Option[Int]]
  eqForType[List[String]]
}
