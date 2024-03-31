package fpinscala.exercises.monoids

import fpinscala.exercises.monoids.Monoid
import fpinscala.exercises.parallelism.Nonblocking.*

import scala.annotation.tailrec

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  // Question 10.1
  lazy val intAddition: Monoid[Int] = new:
    override def combine(a1: Int, a2: Int): Int = a1 + a2
    override def empty: Int = 0

  lazy val intMultiplication: Monoid[Int] = new:
    override def combine(a1: Int, a2: Int): Int = a1 * a2
    override def empty: Int = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    override def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def empty: Boolean = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    override def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def empty: Boolean = true

  // Question 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new:
    override def combine(x: Option[A], y: Option[A]): Option[A] = x orElse y
    override val empty: Option[Nothing] = None

  def firstOption[A]: Monoid[Option[A]] = new:
    override def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def empty: Option[A] = None

  def lastOption[A]: Monoid[Option[A]] = dual(firstOption)

  // We can define the dual combinator to generalise the notion of changing the order of x and y
  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty: A = m.empty

  // Question 10.3
  // Note: a function having the same argument and return type is sometimes called an endofunction.
  // The Greek prefix endo- means within, in the sense that an endofunction's codomain is within its domain
  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(f: A => A, g: A => A) = f andThen g
    def empty = a => a

  import fpinscala.exercises.testing.{Prop, Gen}
  import Gen.`**`

  // laws: associativity, identity
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop.forAll(gen ** gen ** gen):
      case a ** b ** c =>
        m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)

    val identity = Prop.forAll(gen): i =>
      (m.combine(i, m.empty) == i) && (m.combine(m.empty, i) == i)

    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    foldLeft(as)(m.empty)(m.combine)

  // Exercise 10.5: Implement foldMap
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    // combineAll(as.map(f), m)
    // Note: above not optimal as it creates intermediate list
    foldLeft(as)(m.empty): (b, a) =>
      m.combine(b, f(a))

  // Exercise: 10.6: Implement foldRight in terms of foldMap
  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    // Recognise that foldRight's f: (A, B) => B can be passed to
    // foldMap when it is curried, i.e. A => (B => B), along with the
    // endoMonoid we defined earlier. The B type param becomes B => B.
    foldMap(as, dual(endoMonoid))(f.curried)(acc)
    // Think in terms of map-reduce, we map f.curried across `as` to get
    // a List[B => B] then we use the monoid to reduce the list to just
    // a B => B. The endoMonoid combines two functions f: A => B and g: A => A
    // as (f, g) => a => g(f(a)) and provides the empty value as the
    // identity function a => a. By using dual(endoMonoid), we flip the
    // args, so we get f(g(a)).
    // foldMap reduces the list of List[B => B] using foldLeft where the
    // accumulator is initialised as the identity func. Since we flipped
    // the args, this builds a large expression like a => f3(f2(f1(id(a)))).
    // Finally, when we apply the `acc` to the reduced expression, we get B.

  @tailrec
  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    as match
      case head :: tail => foldLeft(tail)(f(acc, head))(f)
      case Nil => acc

  // Exercise 10.7: Implement foldMapV using the balanced folding strategy,
  // e.g. combine(combine(a, b), combine(c, d))
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then
      m.empty
    else if as.length == 1 then
      f(as(0))
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  // Exercise 10.8: Implement a parallel version of foldMap

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    override def combine(a1: Par[A], a2: Par[A]): Par[A] =
      a1.map2(a2)(m.combine)

    override def empty: Par[A] =
      Par.unit(m.empty)

  def parFoldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap: bs =>
      foldMapV(bs, par(m))(a => Par.lazyUnit(a))

  // Exercise 10.9: Use foldMap to detect whether IndexedSeq[Int] is ordered.
  // Hint: you'll need to come up with a creative Monoid
  case class Segment(start: Int, ordered: Boolean)

  val orderedMonoid: Monoid[Segment] = new Monoid[Segment]:
    override def combine(a1: Segment, a2: Segment): Segment =
      val ordered = a1.ordered && a2.start >= a1.start
      Segment(a2.start, ordered)

    override def empty: Segment = Segment(Int.MinValue, true)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMap(ints.toList, orderedMonoid)(a => Segment(a, false)).ordered

  // The above implementation seems to work, but it wasn't what the reference
  // answer did...

  // With this approach, we create a data structure that tracks the min and max
  // values
  case class Interval(ordered: Boolean, min: Int, max: Int)

  // We can combine Intervals because the max of the first interval should be less
  // than or equal to the min of the second interval, given both intervals are
  // ordered. We define the Monoid instance for Option[Interval] since we can use
  // None as the empty identity value
  val intervalMonoid: Monoid[Option[Interval]] = new:
    def combine(a1: Option[Interval], a2: Option[Interval]): Option[Interval] =
      (a1, a2) match
        case (Some(a), Some(b)) =>
          Some(Interval(
            a.ordered && b.ordered && a.max <= b.min,
            a.min,
            b.max
          ))
        case (x, None) => x
        case (None, x) => x

    val empty: Option[Nothing] = None

  def orderedV2(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, intervalMonoid)(i =>
      Some(Interval(true, i, i ))
    ).map(_.ordered).getOrElse(true)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  // Exercise 10.10: Write a Monoid instance for WC and test the monoid laws
  // using our property-based testing library
  lazy val wcMonoid: Monoid[WC] = new Monoid[WC]:
    import WC.*

    override def combine(a1: WC, a2: WC): WC =
      (a1, a2) match
        case (Stub(a), Stub(b)) => Stub(a + b)
        case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
        case (Part(l, w, r), Stub(b)) => Part(l, w, r + b)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          val words = w1 + w2 + (if (r1 + l2).isEmpty then 0 else 1)
          Part(l1, words, r2)

    override def empty: WC = Stub("")

  def wcGen: Gen[WC] =
    val smallString = Gen.choose(0, 10).flatMap(Gen.stringN)
    val genStub = smallString.map(s => WC.Stub(s))
    val genPart = for
      lStub <- smallString
      words <- Gen.choose(0, 10)
      rStub <- smallString
    yield WC.Part(lStub, words, rStub)
    Gen.union(genStub, genPart)

  // see Test below for how to test laws

  // Exercise 10.11: Use the WC monoid to implement the count function
  // Interesting solution: basically convert all whitespace cars into Parts
  // and all the non-whitespace chars into Stubs. This essentially builds
  // the words by essembling all the non-whitespace chars into Stubs
  // between the whitespace chars.
  def count(s: String): Int =
    def wc(c: Char): WC =
      if c.isWhitespace then  WC.Part("", 0, "")
      else WC.Stub(c.toString)

    def unstub(s: String): Int =
      if s.isEmpty then 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
      case WC.Stub(a) => unstub(a)
      case WC.Part(l, w, r) => unstub(l) + w + unstub(r)

  // Exercise 10.16: Implement productMonoid
  // Note: the real power of Monoids is that they composable - if A and B are monoids
  // then (A, B) is also a monoid!
  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) =
      (ma.combine(x(0), y(0)), mb.combine(x(1), y(1)))

    val empty = (ma.empty, mb.empty)

  // Exercise 10.17: Write a monoid instance for functions whose results are monoids
  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  // Exercise 10.18: Implement the bag function to count the number of occurrences of words in the list
  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) =
      foldLeft((a.keySet ++ b.keySet).toList)(empty): (acc, k) =>
        acc.updated(k, mv.combine(
          a.getOrElse(k, mv.empty),
          b.getOrElse(k, mv.empty)
        ))

    val empty = Map()

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    // foldMapV(as, mapMergeMonoid)(a => Map(a -> 1))
    // Note: the above works but we're using foldMapV defined on Monoid which we defined without using a given clause
    // instead, we can import the Foldable instance for IndexedSeq
    import Foldable.given
    given Monoid[Int] = intAddition

    as.foldMap(a => Map(a -> 1))


end Monoid


/*
scala> import fpinscala.exercises.monoids.Test

scala> Test.testOrderMonoid()
true
false
true

scala> Test.testWCMonoid()
+ OK, passed 100 tests.
()

scala> Test.testBag()
Map(red -> 2, green -> 1, blue -> 1, black -> 1)
*/
object Test:
  import Monoid.*

  def testOrderMonoid(): Unit =
    val as = List(1, 2, 3, 4)
    println(ordered(as.toIndexedSeq))

    val as2 = List(1, 4, 3, 2)
    println(ordered(as2.toIndexedSeq))

    println(ordered(List().toIndexedSeq))

  def testWCMonoid(): Unit =
    println(monoidLaws(wcMonoid, wcGen).run())

  def testBag(): Unit =
    println(bag(List("red", "green", "red", "blue", "black").toIndexedSeq))