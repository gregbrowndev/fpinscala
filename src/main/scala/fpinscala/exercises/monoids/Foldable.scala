package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      foldMap(f.curried)(using dual(endoMonoid[B]))(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      foldLeft(mb.empty): (b, a) =>
        mb.combine(b, f(a))

    def combineAll(using ma: Monoid[A]): A =
      foldLeft(ma.empty)(ma.combine)

    def toList: List[A] =
      // foldLeft(Nil : List[A])(_ :+ _)
      // Note: could use above impl - prepending to the end of the list with :+
      // is probably less efficient than appending to the front with ::
      foldRight(Nil: List[A])(_ :: _)

object Foldable:

  // Exercise 10.12: Implement Foldable, Foldable[List], Foldable[IndexedSeq],
  // and Foldable[LazyList]
  given Foldable[List] with
    extension [A](as: List[A])
//      override def foldRight[B](acc: B)(f: (A, B) => B) =
//        ???

      // I've made foldLeft primitive and implemented all other functions in terms of
      // it. Is this the most canonical approach? The book actually implements all
      // functions and creates a cyclic dependency that must be resolved by the
      // implementer of the concrete type.
      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as match
          case h :: t => t.foldLeft(f(acc, h))(f)
          case Nil => acc

      // We can provide a more efficient impl than the base trait
      override def toList: List[A] = as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
//      override def foldRight[B](acc: B)(f: (A, B) => B) =
//        ???

      // Implementation is different to List, notice we don't have :: and Nil
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match
          case h +: t => t.foldLeft(f(acc, h))(f)
          case _ => acc

      // We can provide a more efficient impl of foldMap for IndexedSeq
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as, mb)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      // Here we can delegate foldLeft and foldRight to the underlying data type
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  // Exercise 10.13: Implement Foldable for Tree
  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      // Pretty sure we can skip this implementations
//      override def foldRight[B](acc: B)(f: (A, B) => B) =
//        ???

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match
          case Branch(l, r) => r.foldLeft(l.foldLeft(acc)(f))(f)
          case Leaf(a) => f(acc, a)

      // we can implement balanced folding
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as match
          case Leaf(a) => f(a)
          case Branch(l, r) => mb.combine(l.foldMap(f), r.foldMap(f))

  // Exercise 10.14: Implement Foldable[Option]
  given Foldable[Option] with
    extension [A](as: Option[A])
      // Could omit...
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as match
          case Some(a) => f(a, acc)
          case None => acc

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match
          case Some(a) => f(acc, a)
          case None => acc

      // Could omit overridden impl?
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as match
          case Some(a) => f(a)
          case None => mb.empty
