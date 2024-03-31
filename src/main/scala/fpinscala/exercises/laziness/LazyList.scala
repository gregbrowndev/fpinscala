package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{Empty, cons, empty, unfold}

/*
scala>
import fpinscala.exercises.laziness.LazyList
import LazyList.{Cons, Empty}

scala> LazyList(1, 2, 3, 4).toList
val res1: List[Int] = List(1, 2, 3, 4)

scala> LazyList(1, 2, 3, 4).take(3).toList
val res1: List[Int] = List(1, 2, 3)

scala> LazyList(1, 5, 3, 4).takeWhile2(_ % 2 == 1).toList2
val res1: List[Int] = List(1, 5, 3)

scala> LazyList(1, 5, 3, 4).takeWhile3(_ % 2 == 1).toList
val res1: List[Int] = List(1, 5, 3)

scala> LazyList(1, 5, 3, 4).map(_ + 2).toList
val res1: List[Int] = List(3, 7, 5, 6)

scala> LazyList(1, 5, 3, 4).filter(_ % 2 == 1).toList
val res0: List[Int] = List(1, 5, 3)

scala> LazyList(1, 5, 3, 4).append(5).toList
val res1: List[Int] = List(1, 5, 3, 4, 5)

scala> LazyList(1, 5, 3, 4).append(5.0).toList
val res2: List[Int | Double] = List(1, 5, 3, 4, 5.0)

scala> LazyList(1, 5, 3, 4).flatMap(a => LazyList(a, a * 2)).toList
val res0: List[Int] = List(1, 2, 5, 10, 3, 6, 4, 8)

scala> LazyList.continually(5).take(3).toList
val res0: List[Int] = List(5, 5, 5)

scala> LazyList.from(0).take(10).toList
val res1: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

scala> LazyList.fibs.take(10).toList
val res0: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

scala> LazyList(1, 5, 3, 4).startsWith(LazyList(1, 5))
val res0: Boolean = true

scala> LazyList(1, 5, 3, 4).startsWith(LazyList(1, 4))
val res1: Boolean = false

scala> LazyList(1, 5, 3, 4).startsWith(LazyList())
val res2: Boolean = true

scala> LazyList.fibsViaUnfold.take(8).toList
val res2: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13)

scala> LazyList.fromViaUnfold(0).take(8).toList
val res0: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7)

scala> LazyList.continuallyViaUnfold(5).take(8).toList
val res0: List[Int] = List(5, 5, 5, 5, 5, 5, 5, 5)

scala> LazyList.onesViaUnfold.take(8).toList
val res0: List[Int] = List(1, 1, 1, 1, 1, 1, 1, 1)

scala> LazyList(1, 2, 3, 4).mapViaUnfold(_ + 2).toList
val res1: List[Int] = List(3, 4, 5, 6)

scala> LazyList(1, 2, 3, 4).takeViaUnfold(2).toList
val res0: List[Int] = List(1, 2)

scala> LazyList(1, 2, 3, 4).takeWhileViaUnfold(_ == 1).toList
val res0: List[Int] = List(1)

scala> LazyList(1, 2, 3, 4).zipWith(LazyList(5, 6, 7))(_ + _).toList
val res1: List[Int] = List(6, 8, 10)

scala> LazyList(1, 2, 3, 4).zipAll(LazyList(5, 6, 7)).toList
val res0: List[(Option[Int], Option[Int])] = List((Some(1),Some(5)), (Some(2),Some(6)), (Some(3),Some(7)), (Some(4),None))

scala> LazyList(1, 2, 3, 4).startsWith2(LazyList(1, 2))
val res0: Boolean = true

scala> LazyList(1, 2, 3, 4).startsWith2(LazyList(1, 3))
val res1: Boolean = false

scala> LazyList(1, 2, 3, 4).startsWith2(LazyList(1, 2, 3, 4, 5))
val res2: Boolean = false

scala> LazyList(1, 2, 3, 4).tails.map(_.toList).toList
val res0: List[List[Int]] = List(List(1, 2, 3, 4), List(2, 3, 4), List(3, 4), List(4), List())

scala> LazyList(1, 2, 3, 4).hasSubsequence(LazyList(2, 3))
val res0: Boolean = true

scala> LazyList(1, 2, 3, 4).hasSubsequence(LazyList(2, 4))
val res1: Boolean = false
*/

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    // Note: this is not stack safe!
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList

  def toList2: List[A] =
    @annotation.tailrec
    def go(l: LazyList[A], acc: List[A]): List[A] = l match
      case Empty => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)

    go(this, Nil)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty

  final def take2(n: Int): LazyList[A] = this match
    // Note: this implementation optimises the case when n == 1 by avoiding an unnecessary evaluation of t
    // Note: while this function is not tail recursive, it is stack safe since the recursive call is suspended
    // until the tail of the returned Cons is forced.
    case Cons(h, t) if n > 1 => cons(h(), t().take2(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  def drop(n: Int): LazyList[A] = this match
    case Empty => empty
    case Cons(h, t) =>
      if n == 0 then cons(h(), t())
      else t().drop(n - 1)

  @annotation.tailrec
  final def drop2(n: Int): LazyList[A] = this match
    // Optimal impl.
    // Note: we do not force evaluation of the elements that are dropped
    case Cons(_, t) if n > 0 => t().drop2(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty

  def takeWhile2(p: A => Boolean): LazyList[A] = this match
    // Optimal impl. Forces eval of h only once
    case Cons(h, t) =>
      lazy val head = h()
      if p(head) then cons(head, t().takeWhile2(p))
      else empty
    case _ => empty

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile3(p: A => Boolean): LazyList[A] =
    foldRight(empty: LazyList[A])((a, b) => if p(a) then cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A]){(a, _) => Some(a)}

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B]) { (a, b) =>
      cons(f(a), b)
    }

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty[A]) { (a, b) =>
      if p(a) then cons(a, b)
      else b
    }

  def append[B >: A](b: => B): LazyList[B] =
    foldRight(LazyList(b) : LazyList[B]) { (a, b) => cons(a, b)}

  def concat[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that) { (a, b) => cons(a, b) }

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B]) { (a, b) => f(a).concat(b) }

  /*
  Note: these functions are incremental - they return a LazyList. Nothing actually happens when we call flatMap, the
  elements are only evaluated once we call `toList`. E.g.

  scala> LazyList(1, 5, 3, 4).flatMap(a => LazyList(a, a * 2)).filter(_ % 2 == 0).take(3).toList
  val res3: List[Int] = List(2, 10, 6)

  Trace (bit harder to trace the flatMap and take(n) functions!):

  - LazyList(1, 5, 3, 4).flatMap(a => LazyList(a, a * 2)).filter(_ % 2 == 0).take(3).toList
  - ( append( LazyList(1, 2), LazyList(5, 3, 4).flatMap(a => LazyList(a, a * 2)) ).filter(_ % 2 == 0) ).take(3).toList
  - List(2) ++ (LazyList(5, 3, 4).flatMap(a => LazyList(a, a * 2)).filter(_ % 2 == 0).take(2).toList
  - List(2) ++ ( append(LazyList(5, 10), LazyList(5, 3, 4).flatMap(a => LazyList(a, a * 2)).filter(_ % 2 == 0).take(2).toList
  - List(2, 10) ++ (LazyList(3, 4).flatMap(a => LazyList(a, a * 2)).filter(_ % 2 == 0).take(1).toList
  - List(2, 10) ++ ( append(LazyList(3, 6), LazyList(3, 4).flatMap(a => LazyList(a, a * 2)) ).filter(_ % 2 == 0).take(1).toList
  - List(2, 10, 6) ++ (LazyList(4).flatMap(a => LazyList(a, a * 2)).filter(_ % 2 == 0) ).take(0).toList
  - List(2, 10, 6)
  */

  def startsWith[B](s: LazyList[B]): Boolean = (this, s) match
    case (Cons(h1, t1), Cons(h2, t2)) => if h1() == h2() then t1().startsWith(t2()) else false
    case (_, Cons(_, _)) => false
    case (_, _) => true

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](that: => LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), t1() -> t2())
      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    // Note: Some(None -> None, None -> None) is equiv. to Some((None, None), (None, None))
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(Some(h1()) -> Some(h2()), t1() -> t2())
      case (Cons(h1, t1), Empty) => Some(Some(h1()) -> None, t1() -> empty)
      case (Empty, Cons(h2, t2)) => Some(None -> Some(h2()), empty -> t2())
      case (Empty, Empty) => None
    }

  def startsWith2[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhileViaUnfold(_(1).isDefined).forAll((a, b) => a == b)

  def tails: LazyList[LazyList[A]] =
    // Note: this pattern match uses some new syntax: `s @ <pattern>` which allows us to reuse the original value
    // on the right hand side, so we don't have to recreate the list with cons(h, t())
    unfold(this) {
      case s @ Cons(_, t) => Some(s, t())
      case Empty => None
    }.concat(LazyList(empty))

  /* Now we can implement the hasSubsequence function that we saw in chapter 3 (datastructures) to check whether the
  * list contains a given subsequence. In chapter 3, we were limited to strict lists and list-processing functions. As
  * a result, we had to write this function as a tricky monolithic loop:

    enum List[+A]:
      case Nil
      case Cons(head: A, tail: List[A])

    object List:
      @annotation.tailrec
      def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
        case _ => false

      @annotation.tailrec
      def hasSubsequence[A](l: List[A], prefix: List[A]): Boolean = l match
        case Nil => prefix == Nil
        case s @ Cons(_, t) => startsWith(s, prefix) || hasSubsequence(t, prefix)

  * Our hasSubsequence function on LazyList can generate all the sequences within the list and efficiently check each
  * one if it matches the prefix
  */
  def hasSubsequence[B](that: LazyList[B]): Boolean =
    tails.exists(_.startsWith(that))

  /* Can you use unfold to implement scanRight? Some ideas: unfold is a generative "corecursive" function
  * that generates the next value from the state. If we start with the initial accumulator, b, and unfold the list,
  * we can accumulate the intermediate lists from the left, i.e.

      unfold((LazyList(1, 2, 3), 0) {
        case (Cons(h, t), acc) => Some(acc, (t(), h() + acc))
        case _ => None
      }

    would give us: LazyList(0, 1, 3, 6). Note: not sure if this code is fully sensible.

    The problem is we want to scanRight, i.e. like a foldRight, and end up with: LazyList(6, 5, 3, 0).

    In my code below, I tried to build the initial accumulator using foldRight and then subtract each element from the
    accumulator as we process the list from left to right. However, this doesn't work because we don't have a function
    to subtract A from B, only combine A and B. The function f is additive, whereas we would need to undo the
    addition as we go.

    One way to use unfold here would be to reverse the lazy list first, like we do when implementing foldRight in terms
    of foldLeft. Then we can start from the initial accumulator and add each element from left to right giving us the
    correct answer.

    The problem I see with generalising the tails function above is that tails generates the whole subsequence as it
    goes. It doesn't do any calculations to aggregate the subsequence and reuse the resultant value in the next
    iteration. This wouldn't be helpful anyway, since we don't have a way to subtract elements from the previous state.
  */

//  def scanRight[B](b: => B)(f: (A, => B) => B): LazyList[B] =
//    unfold((this, foldRight(b)(f))) {
//      case (Cons(h, t), acc) =>
//        Some(acc, t() -> (acc - f(h(), acc)))
//      case _ => None
//    }

//  def scanRight[B](b: => B)(f: (A, => B) => B): LazyList[B] =
//    foldRight(LazyList(b)) {
//      case s @ (a, Cons(h, _)) => cons(f(a, h()), s(1))
//    }

  // another way
  def scanRight[B](b: => B)(f: (A, => B) => B): LazyList[B] =
    foldRight((b, LazyList(b))) { (a, b) =>
      lazy val acc = b
      val next = f(a, acc(0))
      (next, cons(next, acc(1)))
    }(1)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def continually2[A](a: A): LazyList[A] =
    // Optimal impl. allocates single cons cells that references itself
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] =
    // Note: like ones and continually, the infinite recursion is safe because the tail is not evaluated until it
    // is forced
    LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    // Again, it seems we don't have to worry about tail recursion since the tail is not evaluated until it is forced?
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  /* From ChatGPT: https://chat.openai.com/share/4ed867f6-6cf1-4e21-9d3a-5a100e57c038

  Question:
  In Scala, when working with lazily evaluated lists, do we not have to worry about tail recursion optimisation and
  the possibility of stack overflow errors in the fib algorithm below?

    lazy val fibs: LazyList[Int] =
      def go(current: Int, next: Int): LazyList[Int] =
        cons(current, go(next, current + next))
      go(0, 1)

  Correct me if I'm wrong. My understanding is that the auxiliary go function returns a new LazyList with the current
  value as the head and a recursive call to compute the tail. Since the tail is not evaluated until its evaluation is
  forced, we don't need to worry about stack safety?

  Answer:
  Your understanding is correct. In Scala, when you work with LazyList (formerly known as Stream), it allows for lazy
  evaluation of elements, which can help avoid stack overflow errors in cases where recursion might be a concern.

  In your fib algorithm, you're defining a LazyList using a recursive function (go). Since the LazyList is evaluated
  lazily, it means that the recursive calls to go are not immediately executed. Instead, they are deferred until you
  actually access the elements of the LazyList.

  Because of this lazy evaluation, you don't need to worry about stack overflow errors when computing Fibonacci numbers
  using this approach. Each recursive call to go generates a new LazyList node with its current value as the head and a
  deferred computation for the tail (the recursive call). This recursive structure is only expanded when you start to
  traverse the LazyList. At that point, the recursion will happen in a depth-first manner, but the evaluation of
  elements will be delayed and won't lead to a stack overflow.

  So, your fib algorithm is indeed stack-safe, thanks to the lazy evaluation provided by LazyList.


  Question:
  Would you not still risk a stack overflow error once you start to expand the lazy list. For example, if I said take
  the first 1000000 elements of the fib series, wouldn't you then get a stack overflow error?

  Answer:
  You are correct; if you attempt to take a very large number of elements from the Fibonacci sequence generated using a
  recursive LazyList, you could still run into a stack overflow error.

  The key point here is that while the LazyList itself is lazily evaluated, once you start to access and traverse its
  elements, the recursion will occur in a depth-first manner. If you try to take a very large number of elements, each
  recursive call will add a new stack frame to the call stack, which could eventually lead to a stack overflow error.

  To avoid this, you can use an iterative approach or memoization to compute Fibonacci numbers for large values
  efficiently without risking a stack overflow.
  */

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (current, next) =>
      Some(current, (next, current + next))
    }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n){ s => Some(s, s+1)}

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    // Note: we pass unit value () for state as it is not required
    unfold(()){ _ => Some(a, ())}

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(()) { _ => Some(1, ()) }
