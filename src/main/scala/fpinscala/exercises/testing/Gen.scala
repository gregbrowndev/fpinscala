package fpinscala.exercises.testing

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.targetName
import fpinscala.exercises.state.{RNG, State}
import Gen.*
import Prop.*
import Prop.Result.{Falsified, Passed, Proved}
import fpinscala.exercises.parallelism.Nonblocking
import fpinscala.exercises.parallelism.Nonblocking.Par


opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  /* Previously, we defined Prop as Either[(FailedCase, SuccessCount), SuccessCount], but then we realised that a piece
  information was missing, we don't know how many test cases to examine before considering the property to have passed.
  We then changed Prop to TestCases => Either[(FailedCase, SuccessCount), SuccessCount]. Since we now have TestCases,
  there is no need for the Right side of the type, so we made Prop: TestCases => Option[(FailedCase, SuccessCount)],
  where None indicates success and Some indicates an error. To make this more explicit, we introduced the Result type
  below.
  */
  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Falsified(_, _) => true
      case _ => false

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  extension (self: Prop)
    def check(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Result = self(maxSize, testCases, rng)

    // helper function to run the Prop and print the results in the REPL
    def run(
       maxSize: MaxSize = 100,
       testCases: TestCases = 100,
       rng: RNG = RNG.Simple(System.currentTimeMillis)
     ): Unit =
      self.check(maxSize, testCases, rng) match
        case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n$msg")
        case Passed => println(s"+ OK, passed $testCases tests.")
        case Proved => println(s"+ OK, proved property")

    def tag(msg: String): Prop =
      (max, n, rng) => self(max, n, rng) match
        case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
        case x => x

    def &&(that: Prop): Prop =
      (max, n, rng) => self.tag("and-left")(max, n, rng) match
          case f @ Falsified(_, _) => f
          case _ => that.tag("and-right")(max, n, rng)

    def ||(that: Prop): Prop =
      (max, n, rng) => self.tag("or-left")(max, n, rng) match
        case Falsified(msg, _) => that.tag("or-right").tag(msg.string)(max, n, rng)
        case x => x

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop:
    (n, rng) =>
      randomLazyList(g)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Passed
              else Falsified(a.toString, i)
            catch
              case e: Exception => Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Passed)

  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt.max(1) + 1

      // Create a list of Props of length min(n, max) using g: SGen[A] as the generator.
      // We have applied the monotonically increasing index from 0 to min(n, max) to the SGen to get a
      // Gen[List[A]] of that size. This is passed into the Prop: forAll Gen[List[A]] the predicate must be true
      val props: LazyList[Prop] =
        LazyList.from(0)
          .take((n.toInt min max.toInt) + 1)
          .map(i => forAll(g(i))(f))

      // We then map each Prop into a new Prop where casesPerSize is used to run the original Prop n number of times
      // This is essentially saying, "forAll Gen[List[A]] pred" run casesPerSize number of times
      // We then reduce the list into a single Prop using the && logical conjunction
      val prop: Prop =
        props.map[Prop](p => (max, _, rng) => p(max, casesPerSize, rng))
          .toList
          .reduce(_ && _)

      // We return the evaluated Prop using continuation-passing style (the Prop hasn't been evaluated at this point yet)
      prop(max, n, rng)

  def verify(p: => Boolean): Prop =
    (_, _, _) => if p then Proved else Falsified("()", 0)

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n${e.getStackTrace.mkString("\n")}"

end Prop

opaque type Gen[+A] = State[RNG, A]

object Gen:
  def apply[A](s: State[RNG, A]): Gen[A] = s

  extension [A](self: Gen[A])
    def run(s: RNG): (A, RNG) = State.run(self)(s)
    def map[B](f: A => B): Gen[B] = State.map(self)(f)
    def map2[B, C](that: Gen[B])(f: (A, B) => C): Gen[C] = State.map2(self)(that)(f)
    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)
    def toOption: Gen[Option[A]] = self.map(Some(_))
    def list: SGen[List[A]] = n => listOfN(n)
    def nonEmptyList: SGen[List[A]] = n => listOfN(n.max(1))
    def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)
    def unsized: SGen[A] = _ => self

    @targetName("product")
    def **[B](gb: Gen[B]): Gen[(A, B)] =
      map2(gb)((_, _))

  extension [A](self: Gen[Option[A]])
    def fromOption: Gen[A] = self.map {
      case Some(a) => a
      case _ => throw Error("Cannot do that")
    }

  def unit[A](a: => A): Gen[A] = State.unit(a)

  val boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  val int: Gen[Int] = Gen(State(RNG.int))
  val double: Gen[Double] = Gen(State(RNG.double))
  
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2 / (g1._2 + g2._2)
    double.flatMap(d => if d < g1Threshold then g1._1 else g2._1)

  def pair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    val intGen = choose(start, stopExclusive)
    intGen.map2(intGen){ (_, _) }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

  object `**`:  // note: backticks are escapes, not part of the object name
    def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)

end Gen


opaque type SGen[+A] = Int => Gen[A]
object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(n: Int): Gen[A] = self(n)

    def map[B](f: A => B): SGen[B] = self(_).map(f)

    def map2[B, C](that: SGen[B])(f: (A, B) => C): SGen[C] =
      s => self(s).map2(that(s))(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      s => self(s).flatMap(f(_)(s))

    @targetName("product")
    def **[B](s2: SGen[B]): SGen[(A, B)] =
      s => Gen.**(apply(s))(s2(s))

  // Examples of concrete SGen values
  val intList: SGen[List[Int]] = n => Gen.int.listOfN(n)
  val doubleList: SGen[List[Double]] = n => Gen.double.listOfN(n)
  val booleanList: SGen[List[Boolean]] = n => Gen.boolean.listOfN(n)


/*
scala>
import fpinscala.exercises.testing.*

scala> Example.runTest()
! Falsified after 0 passed tests:
and-left(...(test case: List()
generated an exception: empty.max
stack trace: ...

// Here we see that the property has failed. The standard library max throws an error for an empty list!

scala> Example.runTest2()
+ OK, passed 100 tests.

scala> Example.runTest3()
+ OK, passed 100 tests.

scala> Example.runTest4()
+ OK, proved property

scala> Example.runTest5()
+ OK, passed 100 tests.

scala> Example.runTest6()
+ OK, passed 100 tests.

scala> Example.runTest7()
+ OK, passed 100 tests.

scala> Example.runTest7()
+ OK, passed 100 tests.

scala> Example.runTest8()
+ OK, passed 100 tests.

scala> Example.runTest9()
+ OK, passed 100 tests.

*/
object Example:
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(smallInt.list): l =>
    val max = l.max
    l.forall(_ <= max)

  def runTest(): Unit = maxProp.run()

  val maxNelProp = Prop.forAll(smallInt.nonEmptyList): l =>
    val max = l.max
    l.forall(_ <= max)

  def runTest2(): Unit = maxNelProp.run()

  val sortedProp = Prop.forAll(smallInt.list): l =>
    val sorted = l.sorted
    val ordered = sorted.isEmpty || sorted.zip(sorted.tail).forall( _ <= _ )
    ordered && sorted.forall(l.contains) && l.forall(sorted.contains)

  def runTest3(): Unit = sortedProp.run()

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    p.map2(p2)(_ == _)

  val executor: ExecutorService = Executors.newCachedThreadPool

  val parProp = Prop.verify:
    val p = Par.unit(1).map(_ + 1)
    val p2 = Par.unit(2)
    // p.run(executor).get == p2.run(executor).get
    equal(p, p2).run(executor)

  def runTest4(): Unit = parProp.run()

  val parPropV2 = Prop.forAll(smallInt): i =>
    equal(
      Par.unit(i).map(_ + 1),
      Par.unit(i + 1)
    ).run(executor)

  def runTest5(): Unit = parPropV2.run()

  // We can introduce forAllPar to move running of Par into a separate function. This also allows us to
  // insert variation across different parallel strategies without cluttering the property!

  val executors: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar_noisy[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors.map2(g)((_, _))): (ex, a) =>
      f(a).run(ex)

  // We can also introduce a ** extension method on Gen to give the product of two generators instead of using map2 in
  // the function above! Additionally, the ** (extractor/unapply) object on Par allows us to use pattern matching

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors ** g):
      case ex ** a => f(a).run(ex)


  val parPropV3 = forAllPar(smallInt): i =>
    equal(
      Par.unit(i).map(_ + 1),
      Par.unit(i + 1)
    )

  def runTest6(): Unit = parPropV3.run()

  // Lastily, we can apply these improvements to create a verifyPar helper

  def verifyPar(p: => Par[Boolean]): Prop =
    forAllPar(unit(()))(_ => p)

  val parPropV4 = verifyPar:
    equal(
      Par.unit(1).map(_ + 1),
      Par.unit(2)
    )

  def runTest7(): Unit = parPropV4.run()

  // Exercise 8.16 - Write a richer generator for Par[Int] that builds more deeply nested parallel computations
  // Note: once we start testing real parallelism with fork, we need to use the Nonblocking implementation, otherwise
  // the executor can deadlock!

  @targetName("forAllParSGen")
  def forAllPar[A](g: SGen[A])(f: A => Par[Boolean]): Prop =
    forAll(SGen(n => executors) ** g):
      case ex ** a =>
        f(a).run(ex)

  def sumPar(xs: IndexedSeq[Int]): Par[Int] =
    if xs.size <= 1 then
      Par.unit(xs.headOption.getOrElse(0))
    else
      val (l, r) = xs.splitAt(xs.size / 2)
      Par.fork(sumPar(l)).map2(Par.fork(sumPar(r)))(_ + _)

  val parPropV5 = forAllPar(smallInt.list): l =>
    equal(
      sumPar(l.toIndexedSeq),
      Par.unit(l.sum)
    )

  def runTest8(): Unit = parPropV5.run()

  // This wasn't what the question was getting at... here is the answer
  val gpy2: Gen[Par[Int]] =
    choose(-100, 100).listOfN(choose(0, 20)).map: ys =>
      ys.foldLeft(Par.unit(0)): (p, y) =>
        Par.fork(p.map2(Par.unit(y))(_ + _))

  val gpy3: Gen[Par[Int]] = smallInt.map(Par.unit)

  // Exercise 8.17 - Express the property about fork, i.e. fork(x) == x

  val forkProp = forAllPar(gpy3): p =>
    equal(Par.fork(p), p)

  def runTest9(): Unit = forkProp.run()