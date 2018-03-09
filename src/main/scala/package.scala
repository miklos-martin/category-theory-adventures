package object category {
  type Id[A] = A

  implicit val functorForId = new Functor[Id] {
    def fmap[A, B] = identity
  }
}
