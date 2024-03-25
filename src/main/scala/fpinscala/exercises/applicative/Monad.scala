package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

trait Monad[F[_]] extends Applicative[F]:
  F =>
  
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    override def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    override def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  override def apply[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    ff.flatMap(f => fa.map(f))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](ffa: F[F[A]])
    def join: F[A] = ffa.flatMap(identity)

  // Exercise 12.11: Try to write compose function where if F[_] and G[_]
  // are monads, then F.compose(G) gives you a monad for F[G[_]]
  // Note: this is not possible but instructive to understand why! This
  // shows that monads don't compose in general like applicatives do!
  def compose[G[_]](G: Monad[G]): Monad[[x] =>> F[G[x]]] = new:
    override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

    extension [A](fga: F[G[A]])
      override def flatMap[B](f: A => F[G[B]]): F[G[B]] = ???
        // F.flatMap(fga)(ga => G.map(ga)(a => f(a)))
        // compile error: Found: G[F[G[B]]], Required: F[G[B]]
    

object Monad:
  // Exercise 12.20: Implement composeM on the Monad companion object
  // Note: Monads don't compose in general, since we do not have a
  // way to handle: F[G[F[G[x]]]] => F[G[x]]. However, if the monad has
  // a traverse instance, then we can sequence F[G[F[..]]] to 
  // get F[F[G[G[x]]]]. Then we can use monad's join the adjacent Fs
  // and Gs to get F[G[x]]
  def composeM[G[_], H[_]](using G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[[x] =>> G[H[x]]] = new:
    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    extension [A](gha: G[H[A]])
      override def flatMap[B](f: A => G[H[B]]): G[H[B]] =
        G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(H.join))
