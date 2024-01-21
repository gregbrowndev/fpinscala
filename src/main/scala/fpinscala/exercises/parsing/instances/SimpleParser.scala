package fpinscala.exercises.parsing.instances

import fpinscala.answers.state.State
import fpinscala.exercises.parsing.{Location, ParseError, Parsers}

import scala.annotation.targetName
import scala.util.matching.Regex


opaque type SimpleParser[+A] = State[Location, Either[ParseError, A]]


object SimpleParser:
  def apply[A](s: State[Location, Either[ParseError, A]]): SimpleParser[A] = s

  @targetName("applyBase")
  def apply[A](s: Location => (Either[ParseError, A], Location)): SimpleParser[A] = SimpleParser(State(s))


object SimpleParsers extends Parsers[SimpleParser]:

  override def string(s: String): SimpleParser[String] =
    SimpleParser: location =>
      if location.remaining.startsWith(s)
      then (Right(s), location.advanceBy(s.length))
      else (Left(location.toError("Expected: " + s)), location)

  override def regex(r: Regex): SimpleParser[String] =
    SimpleParser: location =>
      r.findPrefixOf(location.remaining) match
        case Some(matched) => (Right(matched), location.advanceBy(matched.length))
        case None => (Left(location.toError("No match for regex")), location)

  override def fail(msg: String): SimpleParser[Nothing] =
    SimpleParser: location =>
      (Left(location.toError(msg)), location)

  extension [A](p: SimpleParser[A])

    override def run(input: String): Either[ParseError, A] = ???

    override def attempt: SimpleParser[A] = ???

    override def flatMap[B](f: A => SimpleParser[B]): SimpleParser[B] = ???

    override infix def or(p2: => SimpleParser[A]): SimpleParser[A] = ???

    override def slice: SimpleParser[String] = ???

    override def label(msg: String): SimpleParser[A] = ???

    override def scope(msg: String): SimpleParser[A] = ???

    override def furthest: SimpleParser[A] = ???

    override def latest: SimpleParser[A] = ???
