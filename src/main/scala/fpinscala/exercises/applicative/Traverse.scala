package fpinscala.exercises.applicative

import fpinscala.exercises.monads.Functor
import fpinscala.exercises.state.State
import fpinscala.exercises.monoids.{Monoid, Foldable}
import Applicative.Const

trait Traverse[F[_]] extends Functor[F], Foldable[F]: 
  self =>

  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)

  // Exercise 12.14: Implement map in terms of traverse
  // Note: that we can do this establishes the fact that Traverse extends
  // Functor,  traverse is a generalisation of map.
  // First, we'll need to define the Id monad that we'll use as the
  // applicative in traverse
  type Id[A] = A
  given idMonad: Monad[Id] with
    override def unit[A](a: => A): Id[A] = a
    extension [A](fa: Id[A]) 
      override def flatMap[B](f: A => Id[B]): Id[B] = f(fa)
  
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      // Traversing with the Id type constructor results in the definition 
      // of map!
      fa.traverse[Id, B](f)(using idMonad)

    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      fa.traverse[Const[B, _], Nothing](f)

    // Exercise 12.17: Use mapAccum to give a default impl of foldLeft
    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      fa.mapAccum(acc)((a, s) => ((), f(s, a)))(1)

    override def toList: List[A] =
      fa.mapAccum(List[A]())((a, s) => ((), a :: s))(1).reverse

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverse(a =>
        for
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        yield b
      ).run(s)

    def zipWithIndex: F[(A, Int)] =
      fa.mapAccum(0)((a, s) => ((a, s), s + 1))(0)

    // Exercise 12.16: Implement the reverse function for traversable functors
    // Note: this is very difficult to visualise what is happening 
    def reverse: F[A] =
      fa.mapAccum(fa.toList.reverse)((_, as) => (as.head, as.tail))(0)

    // Exercise 12.18: Implement fuse
    def fuse[M[_], N[_], B](f: A => M[B], g: A => N[B])(
      using m: Applicative[M], n: Applicative[N]
    ): (M[F[B]], N[F[B]]) =
      fa.traverse[[x] =>> (M[x], N[x]), B](a => (f(a), g(a)))(using m.product(n))

  // Exercise 12.19: Implement compose
  def compose[G[_]: Traverse]: Traverse[[x] =>> F[G[x]]] = new:
    extension [A](fga: F[G[A]])
      override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
        self.traverse(fga)(ga => ga.traverse(f))
        
  // Exercise 12.20: Implement composeM on the Monad companion object

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse:
  // Exercise 12.13: Write Traverse instances for List, Option, Tree, and
  // [x] =>> Map[K x]
  given listTraverse: Traverse[List] with
    extension [A](as: List[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
        val g = summon[Applicative[G]]
        as.foldRight(g.unit(List.empty[B]))((a, fb) => f(a).map2(fb)(_ :: _))

  given optionTraverse: Traverse[Option] with
    extension [A](oa: Option[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
        oa match
          case Some(a) => f(a).map(Some(_))
          case _ => summon[Applicative[G]].unit(None)

  given treeTraverse: Traverse[Tree] = new:
    extension [A](ta: Tree[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
        f(ta.head).map2(ta.tail.traverse(a => a.traverse(f)))(Tree(_, _))

      override def map[B](f: A => B): Tree[B] =
        Tree(f(ta.head), ta.tail.map(_.map(f)))
  
  given mapTraverse[K]: Traverse[Map[K, _]] with
    extension [A](m: Map[K, A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
        val g = summon[Applicative[G]]
        m.foldRight(g.unit(Map.empty)):
          case ((k, v), fb) => fb.map2(f(v))((b, a) => b + (k -> a))
