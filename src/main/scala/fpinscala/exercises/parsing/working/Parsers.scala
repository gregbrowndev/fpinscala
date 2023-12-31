package fpinscala.exercises.parsing.working

import fpinscala.exercises.testing.{Gen, Prop}

import scala.annotation.targetName
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]]: 
  /** Parser which consumes one character matching `c`. */
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  /** Parser which consumes a string matching `s`. */
  def string(s: String): Parser[String]

  /** Parser which consumes characters matching the regex. */
  def regex(r: Regex): Parser[String]

  /** Parser which consumes zero or more numeric characters. */
  def digits: Parser[String] = regex("[0-9]+".r)
    
  /** Parser which consumes zero or more numeric characters and converts them to a number. */
  def number: Parser[Int] =
    digits.map(_.toIntOption match
      case Some(n) => n
      case None => throw Error("expected an integer")
    )

  /** Parser which consumes a C/Java style floating point literal, e.g .1, -1.0, 1e9, 1E-23, etc.. */
  def doubleString: Parser[String] =
    regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString.map(_.toDouble)
      
  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = regex("\\s*".r)
  
  def succeed[A](a: A): Parser[A] =
    // This is the same as the unit combinator?
    // Law: succeed(a).run(s) == Right(a)
    string("").map(_ => a)
  
  // Question: Write a parser that recognises zero or more occurrences of `c`
  // and returns the number of occurrences as its value
  def count(c: Char): Parser[Int] =
    // Using the slice combinator, we throw away the Parser[List[Char]]
    // and instead just return Parser[String] which we then call _.size on.
    // This saves performance since String.size is constant time while
    // List.size is O(n) time. Additionally, many.slice.map does not create
    // the intermediate List at all! TODO: why is that? Lazy evaluation?
    char(c).many.slice.map(_.length)

  // Question: Write a parser that recognises one or more occurrences of `c`
  // and returns the number of occurrences as its value
  def count1(c: Char): Parser[Int] =
    char(c).many1.slice.map(_.length)

  // Question: Write a parser that recognises zero or more occurrences of `c1`
  // followed by one or more occurrences of `c2` and returns the pair of
  // occurrences as its value
  def countPair(c1: Char, c2: Char): Parser[(Int, Int)] =
    count(c1) ** count1(c2)

  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) =
    (p._1._1, p._1._2, p._2)

  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) =
    (p._1, p._2._1, p._2._2)

  def charNTimes(c: Char): Parser[String] =
    // Question 9.6: Write a context-sensitive parser using flatMap.
    // Parse the input to find a digit, n, then parse the char `c` n times
    number.flatMap(string(c.toString).listOfN(_).slice)

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r)

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]
    
    def attempt: Parser[A]

    def map[B](f: A => B): Parser[B] =
      // Question 9.8: Implement map in terms of flatMap and/or other combinators
      // Law: p.map(a => a) == p
      // In terms of map2...
      // p.map2(succeed(Nil))((a, _) => f(a))

      // In terms of flatMap
      p.flatMap(a => succeed(f(a)))

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      // Question 9.1: Implement map2 using product
      // Note: we can make map2 primitive and define product in terms of map2,
      // like we have in previous chapters. However, to do this, you need flatMap
      // (I think)...
//      p.product(p2).map: (a, b) =>
//        f(a, b)
//
      // Question 9.7: Implement map2 in terms of flatMap
      for
        a <- p
        b <- p2
      yield f(a, b)
    
    def flatMap[B](f: A => Parser[B]): Parser[B]

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      // Question 9.7 (continued): Implement product in terms of flatMap
      for
        a <- p
        b <- p2
      yield (a, b)

//    @targetName("product")
    def **[B](p2: => Parser[B]): Parser[(A, B)] = p.product(p2)

    // Question 9.2: Define the laws for product
    // Law: p ** p == (p, p)
    // Law: (a ** (b ** c)).map(unbiasL) == ((a ** b) ** c).map(unbiasR)

    // Note: without unbiasL and unbiasR, the above law wouldn't be true
    // since it would state (A, (B, C)) == ((A, B), C). We sometimes use
    // the ~= equality when there is an obvious bijection between the two
    // sides... e.g.
    // Law: (a ** (b ** c)) ~= ((a ** b) ** c)

    // map and product have an interesting relationship - we can map either
    // before or after taking the product without affecting the behaviour:
    // a.map(f) ** b.map(g) == (a ** b).map((a, b) => (f(a), g(b)))

    infix def or(p2: => Parser[A]): Parser[A]

//    @targetName("or")
    def |(p2: => Parser[A]): Parser[A] = p or p2

    @targetName("discardLeft")
    def *>[B](p2: => Parser[B]): Parser[B] =
      p.slice.map2(p2)((_, b) => b)
      
    @targetName("discardRight")
    def <*[B](p2: => Parser[Any]): Parser[A] =
      p.map2(p2.slice)((a, _) => a)

    
    def as[B](b: B): Parser[B] = p.slice.map(_ => b)
      
    /** Attempts `p` and consumes any trailing whitespace characters. */
    def token: Parser[A] =
      p.attempt <* whitespace

    /** Zero or more repetitions of `p`, separated by `separator` (omitted in the result). */
    def sep(separator: Parser[Any]): Parser[List[A]] =
      p.sep1(separator) | succeed(Nil)

    /** One or more repetitions of `p`, separated by `separator` (omitted in the result). */
    def sep1(separator: Parser[Any]): Parser[List[A]] =
      p.map2((separator *> p).many)(_ :: _)
    
    def listOfN(n: Int): Parser[List[A]] =
      // Question 9.4: Implement listOfN using map2 and succeed
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def slice: Parser[String]

    def many: Parser[List[A]] =
      // Question 9.3: Implement many in terms of |, map2, and succeed
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      // Question 9.1 (continued): Implement many1 using map2
      // Note: this works because the first arg in map2, p, will
      // consume the char in the input string, then the second argument,
      // p.many, will consume zero or more occurrences from the remaining
      // input. It wasn't obvious to me that this would be the case.
      p.map2(p.many)(_ :: _)

    /** The root of the grammar, expects no further input following `p`. */
    def root: Parser[A] =
      p <* eof

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in): s =>
        p1.run(s) == p2.run(s)

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)


