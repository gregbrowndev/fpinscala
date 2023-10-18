package fpinscala.exercises.state

import scala.annotation.tailrec

/*
scala>
import fpinscala.exercises.state.RNG.nonNegativeInt

scala> nonNegativeInt(RNG.Simple(1))
val res2: (Int, fpinscala.exercises.state.RNG) = (384748,Simple(25214903928))

scala> RNG.ints(5)(RNG.Simple(1))
val res4: (List[Int], fpinscala.exercises.state.RNG) = (List(384748, -1151252339, -549383847, 1612966641, -883454042),Simple(25214903928))

scala> RNG.map2(RNG.unit(1), RNG.unit(4))( _ + _ )(RNG.Simple(1))
val res2: (Int, fpinscala.exercises.state.RNG) = (5,Simple(1))

scala> RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(RNG.Simple(1))
val res0: (List[Int], fpinscala.exercises.state.RNG) = (List(1, 2, 3),Simple(1))

scala> RNG.intsViaSequence(5)(RNG.Simple(1))
val res0: (List[Int], fpinscala.exercises.state.RNG) = (List(384748, -1151252339, -549383847, 1612966641, -883454042),Simple(223576932655868))

scala> RNG.flatMap(RNG.unit(5))( a => RNG.unit(a * 2))(RNG.Simple(1))
val res0: (Int, fpinscala.exercises.state.RNG) = (10,Simple(1))

scala> RNG.mapViaFlatMap(RNG.unit(5))(_ * 2)(RNG.Simple(1))
val res1: (Int, fpinscala.exercises.state.RNG) = (10,Simple(1))

scala> Candy.simulateMachine(List(Input.Coin, Input.Turn)).run(Machine(true, 10, 0))
val res1: ((Int, Int), fpinscala.exercises.state.Machine) = ((1,9),Machine(true,9,1))
*/

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i1, rng2) = rng.nextInt
    val result = if i1 == Int.MinValue then Int.MaxValue else Math.abs(i1)
    (result, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (i1, rng2) = nonNegativeInt(rng)
    (i1 / (Int.MaxValue.toDouble + 1), rng2)


  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i1, rng2) = rng.nextInt
    val (d1, rng3) = double(rng2)
    (i1 -> d1, rng3)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (i1, rng2) = rng.nextInt
    val (d1, rng3) = double(rng2)
    (d1 -> i1, rng3)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

  // Note: using unfold didn't quite work as it doesn't return the final state only the list of ints
//  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
//    List.unfold((rng, count)) {
//      case (r, n) if n > 0 =>
//        val (i1, r2) = r.nextInt
//        Some(i1, (r2, n - 1))
//      case _ => None
//    }

  // Got unfold to work by generate a List[(Int, RNG)] then foldRight to accumulate
  // the List[Int] and last RNG. However, this is not very nice looking
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    List.unfold((rng, count)) {
      case (r, n) if n > 0 =>
        val (i1, r2) = r.nextInt
        Some((i1, r2), (r2, n - 1))
      case _ => None
    }.foldRight((List.empty[Int], rng)) {
      case ((i, r), (acc, _)) => (i :: acc, r)
    }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def go(n: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if n <= 0 then (xs, r)
      else
        val (x, r2) = r.nextInt
        go(n - 1, r2, x :: xs)

    go(count, rng, Nil)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt) { a => a / (Int.MaxValue.toDouble  + 1) }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A])) { (rng, acc) =>
      map2(rng, acc) { (a, l) => a :: l }
    }

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
//    l.foldRight(unit[S, List[A]](Nil)) { (s, acc) =>
//      s.map2(acc)(_ :: _)
//    }
    // Now we have traverse, we can simplify this code
    traverse(l)(a => a)

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil)) { (a, acc) =>
      f(a).map2(acc)(_ :: _)
    }

  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s2) = underlying(s)
        f(a)(s2)


enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  // With map + sequence:
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
//    for
//      _ <- State.sequence(inputs.map(i => State.modify(update(i, _))))
//      s <- State.get
//    yield (s.coins, s.candies)
//
//  def update(i: Input, s: Machine): Machine =
//    (i, s) match
//      case (Input.Coin, Machine(_, 0, _)) => s
//      case (Input.Coin, Machine(false, _, _)) => s
//      case (Input.Turn, Machine(true, _, _)) => s
//      case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
//      case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)


  // With traverse:
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
//    for
//      _ <- State.traverse(inputs)(i => State.modify(update(i, _)))
//      s <- State.get
//    yield (s.coins, s.candies)
//
//  def update(i: Input, s: Machine): Machine =
//    (i, s) match
//      case (Input.Coin, Machine(_, 0, _)) => s
//      case (Input.Coin, Machine(false, _, _)) => s
//      case (Input.Turn, Machine(true, _, _)) => s
//      case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
//      case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)


  // With traverse + currying
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))  // Note: curried function without placeholder
      s <- State.get
    yield (s.coins, s.candies)

  // Curried update function
  val update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match
      case (Input.Coin, Machine(_, 0, _)) => s
      case (Input.Coin, Machine(false, _, _)) => s
      case (Input.Turn, Machine(true, _, _)) => s
      case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
