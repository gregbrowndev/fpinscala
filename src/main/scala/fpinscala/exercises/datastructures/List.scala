package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Cons(_, xs) => xs
      case Nil => Nil

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Cons(_, xs) => Cons(h, xs)
      case Nil => Cons(h, Nil)

  def drop[A](l: List[A], n: Int): List[A] =
    l match
      case Nil => Nil
      case Cons(_, xs) =>
        if n == 0 then l
        else drop(xs, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil => Nil
      case Cons(x, xs) =>
        if f(x) then dropWhile(xs, f)
        else l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => 1 + acc)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil : List[A], (acc, x) => Cons(x, acc))

  def foldRightV2[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    // See book for interesting solutions by using foldLeft to build a function
    // of B => B which we then apply the initial acc to get the final value
    foldLeft(reverse(l), acc, (b, a) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (a, acc) => Cons(a, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], append)

  def incrementEach(l: List[Int]): List[Int] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, incrementEach(xs))

  def incrementEachViaFold(l: List[Int]): List[Int] =
    foldRight(l, Nil : List[Int], (a, acc) => Cons(a + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, doubleToString(xs))

  def doubleToStringViaFold(l: List[Double]): List[String] =
    foldRight(l, Nil : List[String], (a, acc) => Cons(a.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs, f))

  def mapViaFold[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil : List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil => Nil
      case Cons(x, xs) =>
        if f(x) then Cons(x, filter(xs, f))
        else filter(xs, f)

  def filterViaFold[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil : List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    as match
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs, f))
//      case Cons(x, xs) => flatMap(xs, a => append(f(x), f(a)))

  def flatMapViaFold[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil : List[B], (a, acc) => append(f(a), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))

  def zipWith2[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    // Not stack safe
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith2(t1, t2, f))

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    @annotation.tailrec
    def loop(a: List[A], b: List[B], acc: List[C]): List[C] =
      (a, b) match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))

    reverse(loop(a, b, Nil))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    def go(l: List[A], s: List[A]): Boolean =
      (l,s) match
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) => if h1 == h2 then go(t1, t2) else false

    if length(sup) == 0 then false
    else if go(sup, sub) then true
    else hasSubsequence(tail(sup), sub)