file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/05-state/Exercises.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1240
uri: file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/05-state/Exercises.scala
text:
```scala
// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, rng2) = rng.nextInt
    val nonNegative = if i < 0 then -(i + 1) else i
    (nonNegative, rng2)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (i, rng2) = nonNegativeInt(rng)
    val d = i (@@)

  // Exercise 3
  
  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): Any = 
    ???

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): Any = 
    ???

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): Any = 
    ???


  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    ???

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    ???

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ??? 

  def ints2(size: Int): Rand[List[Int]] =
    ???

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    ???

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    ???

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    ???

  def map[B](f: A => B): State[S, B] = 
    ???

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    ???


object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    ???

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] =
    ???

  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 
    ???

  lazy val tenStrictInts: List[Int] = 
    ???

end State

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:131)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.countParams(Signatures.scala:501)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:186)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:94)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:63)
	scala.meta.internal.pc.MetalsSignatures$.signatures(MetalsSignatures.scala:17)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:51)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0