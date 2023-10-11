package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(r) => Right(f(r))
    case Left(e) => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(r) => f(r)
    case Left(e) => Left(e)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Right(r) => Right(r)
    case _ => b

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil) : Either[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    (a, b) match
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
      case (Left(v1), Right(_)) => Left(v1)
      case (Right(_), Left(v2)) => Left(v2)
      case (Left(v1), Left(v2)) => Left(v1 ++ v2)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldRight(Right(Nil) : Either[List[E], List[B]])((a, acc) => map2All(f(a), acc, _ :: _))

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(as, identity)

/*
scala>
import fpinscala.exercises.errorhandling.Either
import Either.{Right, Left}

scala> Either.traverse(List(Right(1), Right(2), Right(3)))(a => a.map(_ + 2))
val res2: fpinscala.exercises.errorhandling.Either[Nothing, List[Int]] = Right(List(3, 4, 5))

scala> Either.sequence(List(Right(1), Left("Some error!"), Right(2), Right(3)))
val res1: fpinscala.exercises.errorhandling.Either[String, List[Int]] = Left(Some error!)


scala> val errors: List[Either[List[String], Int]] = List(Right(1), Right(2), Right(3))
val errors: List[fpinscala.exercises.errorhandling.Either[List[String], Int]] = List(Right(1), Right(2), Right(3))

scala> Either.sequenceAll(errors)
val res0: fpinscala.exercises.errorhandling.Either[List[String], List[Int]] = Right(List(1, 2, 3))

scala> val errors: List[Either[List[String], Int]] = List(Right(1), Right(2), Right(3), Left(List("Some error")), Left(List("Another error", "And another")))
val errors: List[fpinscala.exercises.errorhandling.Either[List[String], Int]] = List(Right(1), Right(2), Right(3), Left(List(Some error)), Left(List(Another error, And another)))

scala> Either.sequenceAll(errors)
val res1: fpinscala.exercises.errorhandling.Either[List[String], List[Int]] = Left(List(Some error, Another error, And another))
*/