package category

import org.scalacheck.Properties

object IdAsFunctor extends Properties("Id functor") with FunctorLaws {
  proveFunctorLaws[Id]
}
