package fpinscala.exercises.testing

import fpinscala.exercises.state.{RNG, State}

object Exercise5:

  opaque type Gen[+A] = State[RNG, A]
  
  object Gen:
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
      
    def unit[A](a: => A): Gen[A] = State.unit(a)
    
    def boolean: Gen[Boolean] = State(RNG.boolean)
    
    def double: Gen[Double] = State(RNG.double)

    // Exercise 7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if b then g1 else g2)
      
    // Exercise 8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      val g1Threshold = g1._2 / (g1._2 + g2._2)
      double.flatMap(d => if d < g1Threshold then g1._1 else g2._1)
      
    
    extension [A](self: Gen[A])
      
      def map[B](f: A => B): Gen[B] = State.map(self)(f)
      
      def map2[B, C](that: Gen[B])(f: (A, B) => C): Gen[C] = State.map2(self)(that)(f)
      
      // Exercise 6
      def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)
      
      def listOfN(n: Int): Gen[List[A]] =
        State.sequence(List.fill(n)(self))

      // Exercise 6
      def listOfN(size: Gen[Int]): Gen[List[A]] =
        size.flatMap(listOfN)
        
        
    // Questions for the reader
    
    // If we can generate a single Int in some range, do we need a new primitive to generate an (Int, Int) pair
    // in some range? - Yes: we need map2 to transform (Gen[Int], Gen[Int]) into Gen[(Int, Int)]
    def pair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
      val intGen = choose(start, stopExclusive)
      intGen.map2(intGen){ (_, _) }
      
    
    // Can we produce a Gen[Option[A]] from a Gen[A]? What about a Gen[A] from a Gen[Option[A]]?
    extension [A](self: Gen[A])
      
      def toOption: Gen[Option[A]] =
        self.map(Some(_))
      
    extension [A](self: Gen[Option[A]])  
      def fromOption: Gen[A] = self.map {
        case Some(a) => a
        case _ => throw Error("Cannot do that")
      }