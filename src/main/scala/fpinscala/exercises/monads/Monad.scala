package fpinscala.exercises
package monads

import parsing.*
import testing.*
import parallelism.*
import state.*
import parallelism.Par.*

import scala.::

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)])
    def distribute: (F[A], F[B]) =
      (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]])
    def codistribute: F[Either[A, B]] =
      e match
        case Left(fa) => fa.map(Left(_))
        case Right(fb) => fb.map(Right(_))

object Functor:
  given listFunctor: Functor[List] with
    extension [A](as: List[A])
      def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join
    
    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

  // Exercise 11.3: Implement sequence and traverse
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(Nil)): (a, b) =>
      f(a).map2(b)(_ :: _)

  // Exercise 11.4: Implement replicateM
  // Note: in previous chapters this was called listOfN. Now we call it
  // replicateM for "replicate in a monad". See REPL output at bottom of
  // the file for example usage.
  //
  // Note: replicateM behaves wildly differently for different Monads,
  // e.g. List and Option. However we can say:
  //
  // replicateM repeats the  supplied monadic value n times, combining
  // the results into a single value, where there monadic type defines
  // how that combination is performed...
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // Exercise 11.7: Implement the Kleisli composition function compose
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  // Exercise 11.9: Show the two formulations of the associative law for
  // Monads are equivalent.
  // Using compose, we can now express the associative law for Monads in
  // a more symmetric way:
  // compose(compose(f, g), h) == compose(f, compose(g, h))
  //
  // as opposed to what we saw before:
  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
  //
  // Lets plug the definition of compose into the compose formulation:
  // a => compose(f, g)(a).flatMap(h) ==
  //    a => f(a).flatMap(compose(g, h))
  //
  // a => (b => f(b).flatMap(g))(a).flatMap(h) ==
  //    a => f(a).flatMap(b => g(b).flatMap(h))
  //
  // a => f(a).flatMap(g).flatMap(h) ==
  //    a => f(a).flatMap(b => g(b).flatMap(h)
  //
  // replace a => f(a) on both sides with arbitrary x
  //
  // x.flatMap(g).flatMap(h) == x.flatMap(b => g(b).flatMap(h))

  // Exercise 11.10: Prove the two formulations of the left and right
  // identity laws are equivalent
  //
  // We have the compose formulation:
  //   compose(f, unit) == f   , i.e. right identity law
  //   compose(unit, f) == f   , i.e. left identity law
  //
  // and the flatMap formulation:
  //   x.flatMap(unit) == x
  //   unit(y).flatMap(f) == f(y)
  //
  // let's prove the right identity law first:
  //   compose(f, unit) == f                 , right identity law
  //   a => f(a).flatMap(unit) == f          , sub for compose
  //   a => f(a).flatMap(unit) == a => f(a)  , introduce anonymous func on right
  //   x.flatMap(unit) == x                  , sub x = f(a) on both sides
  //
  // and the left identity law:
  //   compose(unit, f) == f                 , i.e. left identity law
  //   a => unit(a).flatMap(f) == f          , sub for compose
  //   (a => unit(a).flatMap(f))(y) == f(y)  , apply y to both sides
  //   unit(y).flatMap(f) == f(y)            , simplify

  // Exercise 11.11: Prove that the identity laws hold for a Monad of your
  // choice. Let's look at Option
  // Right identity law:
  //   x.flatMap(unit) == x
  //   None.flatMap(unit) == None
  //   None == None
  //
  //   Some(1).flatMap(unit) == Some(1)
  //   (a => unit(a))(1) == Some(1)
  //   unit(1) == Some(1)
  //   Some(1) == Some(1)
  //
  // Left identity law:
  //   unit(y).flatMap(f) == f(y)
  //   Some(y).flatMap(f) == f(y)  , apply def of unit
  //   f(y) == f(y)  , apply def of flatMap

  extension [A](fa: F[A])
    // Exercise 11.8: Implement flatMap in terms of compose
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      compose(_ => fa, f)(())

  // Exercise 11.6: Implement filterM
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(Nil)): (a, fb) =>
      f(a).map2(fb): (p, b) =>
        if p then a :: b else b

  /*
  scala> summon[Monad[Option]].filterM(List(1, 2, 3, 4, 5))(a => Some(a % 2 == 0))
  val res0: Option[List[Int]] = Some(List(2, 4))

  scala> summon[Monad[Option]].filterM(List(1, 2, 3, 4, 5))(a => None)
  val res1: Option[List[Int]] = None

  scala> summon[Monad[List]].filterM(List(2, 3, 4))(a => List())
  val res2: List[List[Int]] = List()

  scala> summon[Monad[List]].filterM(List(2, 3, 4))(a => List(a % 2 == 0))
  val res3: List[List[Int]] = List(List(2, 4))

  scala> summon[Monad[List]].filterM(List(2, 3, 4))(a => List(a % 2 == 0, a % 3 == 0))
  val res4: List[List[Int]] = List(List(2, 4), List(2), List(2, 3, 4), List(2, 3), List(4), List(), List(3, 4), List(3))

  Pretty odd...
  */

  // Exercise 11.12: Implement join in terms of flatMap
  extension [A](ffa: F[F[A]]) def join: F[A] =
    ffa.flatMap(identity)

  // Exercise 11.13: Implement flatMap and compose in terms of join and map
  // Note: I think we're supposed to ignore the implementation of join that we
  // defined above...
  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).map(g).join

  // Exercise 11.14: Restate the monad laws to only mention join, map, and unit
  // Remember laws are Associativity (1), Right Identity (2), and Left Identity (3)
  // (1):   x.flatMap(f).flatMap(g)      == x.flatMap(a => f(a).flatMap(g))
  //         (x.map(f).join).map(g).join == x.map(a => f(a).map(g).join).join
  // (2):   x.flatMap(unit) == x
  //          x.map(unit).join == x
  // (3):   unit(y).flatMap(f) == f(y)
  //          unit(y).map(f).join == f(y)

  // Exercise 11.15: Write down an explanation in your own words what the
  // associativity law means for Par and Parser
  //
  // The Monad law of associativity means that it doesn't matter how we
  // sequence together two Pars (parallel operations) or two Parsers. More
  // specifically, we can apply function f to some value in a Par context
  // and then apply function g to the result of that operation, or we can
  // apply g to the result of f(a) within the first flatMap.

  // Exercise 11.16: State in concrete terms what the identity laws say
  // Gen and List
  //
  // The identity of Gen is a function that takes a value and returns the
  // same value, i.e. A => Gen[A], while the identity of List is a => List(a).
  // The right identity law means that flatMap cannot itself change the
  // structure of the monad, i.e. applying a => List(a) to every item in the
  // list simply returns the same list. The left law means that the value
  // you get from unit(y), e.g. List(y), and which some arbitrary function f is
  // then applied, note f must have type A => F[B], is the same as the value
  // you'd get if you just applied f to y, i.e. f(y), meaning travelling through
  // the unit generator and flatMap has no effect on the values.

end Monad      

object Monad:
  // Exercise 11.1: Implement monad instance for Par, Parser, Option,
  // LazyList, and List
  given genMonad: Monad[Gen] with
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(fa)(f)

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A) = p.succeed(a)
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        p.flatMap(fa)(f)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A) = Some(a)
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]) =
        fa.flatMap(f)

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: => A) = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        fa.flatMap(f)

  given listMonad: Monad[List] with
    def unit[A](a: => A) = List(a)
    extension [A](fa: List[A])
      override def flatMap[B](f: A => List[B]) =
        fa.flatMap(f)

  // Exercise 11.2: Try to implement the State monad
  // Note: the State type constructor takes two types while Monad
  // only expects one... we'll discuss this later in the chapter
  type IntState[A] = State[Int, A]
  given stateIntMonad: Monad[IntState] with
    override def unit[A](a: => A): IntState[A] =
      State.unit(a)
    extension [A](fa: IntState[A])
      override def flatMap[B](f: A => IntState[B]) =
        State.flatMap(fa)(f)

  // We can define the State monad for any S using an anonymous type
  // constructor (a type lambda in Scala 2)
  // Note: State[S, _] is short for [x] =>> State[S, x]
  given stateMonad[S]: Monad[State[S, _]] with
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)

  // Exercise 11.18: See how the State monad behaves, e.g. what is the
  // meaning of replicateM, map2, sequence?


end Monad

case class Id[+A](value: A):
  // Exercise 11.17: Implement map and flatMap
  def map[B](f: A => B): Id[B] =
    Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)

object Id:
  // Exercise 11.17 (continued): Implement the Id monad
  given idMonad: Monad[Id] with
    def unit[A](a: => A) = Id(a)
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]) =
        fa match
          case Id(value) => f(value)

// Exercise 11.20: Define a monad instance for the Reader type
// Explain what it means. What are its primitive operations? What is
// the action of flatMap? What meaning does it give to monadic functions
// like sequence, join, and replicateM? What meaning does it give to the
// monadic laws?
opaque type Reader[-R, +A] = R => A

object Reader:
  def read[R]: Reader[R, R] = r => r
  
  extension [R, A](ra: Reader[R, A])
    def run(r: R): A = ra(r)

  given readerMonad[R]: Monad[Reader[R, _]] with
    def unit[A](a: => A): Reader[R, A] = _ => a
    extension [A](fa: Reader[R, A])
      override def flatMap[B](f: A => Reader[R, B]) =
        r => f(fa(r))(r)


/*
scala> import fpinscala.exercises.monads.{Test, Monad}

// Test the replicateM function:

scala> summon[Monad[List]].replicateM(1, List(1))
val res0: List[List[Int]] = List(List(1))

scala> summon[Monad[List]].replicateM(2, List(1))
val res1: List[List[Int]] = List(List(1, 1))

scala> summon[Monad[List]].replicateM(3, List(1))
val res2: List[List[Int]] = List(List(1, 1, 1))

scala> summon[Monad[List]].replicateM(1, List(1, 2))
val res3: List[List[Int]] = List(List(1), List(2))

scala> summon[Monad[List]].replicateM(2, List(1, 2))
val res4: List[List[Int]] = List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))

scala> summon[Monad[Option]].replicateM(1, Some(1))
val res7: Option[List[Int]] = Some(List(1))

scala> summon[Monad[Option]].replicateM(1, Some(0))
val res8: Option[List[Int]] = Some(List(0))

scala> summon[Monad[Option]].replicateM(1, None)
val res9: Option[List[Nothing]] = None

scala> summon[Monad[Option]].replicateM(2, Some(0))
val res10: Option[List[Int]] = Some(List(0, 0))

scala> summon[Monad[Option]].replicateM(3, Some(0))
val res11: Option[List[Int]] = Some(List(0, 0, 0))
*/
object Test:
  import Monad.given
  def testReplicateM(): Unit =
    val result = listMonad.replicateM(3, List(1, 2, 3))
    println(result)
