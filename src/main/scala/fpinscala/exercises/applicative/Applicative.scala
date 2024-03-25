package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

trait Applicative[F[_]] extends Functor[F]:
  self =>

  def unit[A](a: => A): F[A]

  // Exercise 12.2 (continued): Show that apply can be implemented in terms
  // of map2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)((f, a) => f(a))

  extension [A](fa: F[A])
    // Exercise 12.2: Show that unit and apply form an alternative set of
    // primitives for Applicative. Implement map and map2 in terms of them.
    def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(fa.map(f.curried))(fb)

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  // Exercise 12.1: Implement sequence, traverse, replicateM, and product
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(Nil))((a, fb) => f(a).map2(fb)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  extension [A](fa: F[A])
    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

    // Exercise 12.3: Implement map3 and map4 using apply
    def map3[B, C, D](
      fb: F[B],
      fc: F[C]
    )(f: (A, B, C) => D): F[D] =
      // apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
      // Note: we can use map(n-1)...
      apply(fa.map2(fb)((a, b) => f(a, b, _)))(fc)

    def map4[B, C, D, E](
      fb: F[B],
      fc: F[C],
      fd: F[D]
    )(f: (A, B, C, D) => E): F[E] =
      // apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
      // Or...
      apply(fa.map3(fb, fc)((a, b, c) => f(a, b, c, _)))(fd)

  // Exercise 12.8: Implement product
  // Note: just like we can take the product of two monoids A and B to
  // get the monoid (A, B), we can take the product of two applicative
  // functors
  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] = new:
    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
      (self.apply(fs(0))(p(0)), G.apply(fs(1))(p(1)))

  // Exercise 12.9: Implement compose
  // Note: if F[_] and G[_] are applicative functors, then so is F[G[_]]
  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    extension [A](fga: F[G[A]])
      override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga)(fgb)(G.map2(_)(_)(f))

  // Exercise 12.12: Implement sequence for the Map data type
  // Note: the Applicative trait defined sequence for the List data type,
  // but much like how there are data types other than List that are
  // Foldable, there are data types other than List that are Traversable!
  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty)):
      case (fb, (k, fv)) => fb.map2(fv)((b, v) => b + (k -> v))

object Applicative:
  opaque type ZipList[+A] = LazyList[A]

  object ZipList:
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    extension [A](za: ZipList[A]) def toLazyList: LazyList[A] = za

    given zipListApplicative: Applicative[ZipList] with
      def unit[A](a: => A): ZipList[A] =
        LazyList.continually(a)
      extension [A](fa: ZipList[A])
        override def map2[B, C](fb: ZipList[B])(f: (A, B) => C) =
          fa.zip(fb).map(f.tupled)

  enum Validated[+E, +A]:
    case Valid(get: A) extends Validated[Nothing, A]
    case Invalid(error: E) extends Validated[E, Nothing]
  
  object Validated:
    // Exercise 12.6: Implement an applicative instance for Validated.
    // Note: In chapter 4, we modified the signature of map2 to include an
    // extra 'combineErrors' function. Here, we have added the constraint
    // E: Monoid so we can accumulate errors using the monoid instance
    given validatedApplicative[E: Monoid]: Applicative[Validated[E, _]] with
      def unit[A](a: => A) = Valid(a)
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) = (fa, fb) match
          case (Valid(a), Valid(b)) => unit(f(a, b))
          case (Invalid(e1), Invalid(e2)) =>
            val m = summon[Monoid[E]]
            Invalid(m.combine(e1, e2))
          case (e @ Invalid(_), _) => e
          case (_, e @ Invalid(_)) => e

  type Const[A, B] = A

  given monoidApplicative[M](using m: Monoid[M]): Applicative[Const[M, _]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M = m.combine(m1, m2)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](oa: Option[A])
      override def flatMap[B](f: A => Option[B]) = oa.flatMap(f)

  // Exercise 12.5: Write a monad instance for Either
  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]) = eea match
        case Left(e) => Left(e)
        case Right(a) => f(a)

  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
