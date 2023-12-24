package fpinscala.exercises.testing

import fpinscala.exercises.state.{RNG, State}

object Exercise4:

  opaque type Gen[+A] = State[RNG, A]
  
  object Gen:
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))