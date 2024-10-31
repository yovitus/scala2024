// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.prop

import adpro.state.*

val TODO = 42

// Exercise 1

lazy val rng1: RNG = RNG.Simple(42)


// Exercise 2

lazy val (x, rng2): (Int, RNG) = rng1.nextInt
lazy val y = rng2.nextInt._1

// Exercise 3

object Exercise_3:
  
  import org.scalacheck.Prop.{forAll, forAllNoShrink}
 
  // Use this generator explicitly in the two properties
  val intList = 
    org.scalacheck.Gen.listOf(org.scalacheck.Gen.choose(0, 100))
      .suchThat (_.nonEmpty)

  // The properties are put into a function taking `minimum` as argument to
  // allow the teachers to test them with different mutants of `minimum`.
  
  def p1Min (minimum: List[Int] => Int): org.scalacheck.Prop = 
     org.scalacheck.Prop.forAll(intList) { l => 
      val min = minimum(l)
      l.forall(_ >= min)  
    }

  def p2Min (minimum: List[Int] => Int): org.scalacheck.Prop = 
    org.scalacheck.Prop.forAllNoShrink(intList) { l => 
      val min = minimum(l)
      l.exists(_ == min)  
    }

end Exercise_3

// Exercise 4

object Exercise_4:

  // This implementation of Prop is only used in Exercise 4
  trait Prop:
    self => // TODO remove
    def check: Boolean

    infix def && (that: Prop): Prop =
      new Prop:
        def check = self.check && that.check

end Exercise_4

opaque type Gen[+A] = State[RNG, A]

object Gen:
  
  extension [A](g: Gen[A]) 
    // Let's convert generator to streams of generators
    def toLazyList (rng: RNG): LazyList[A] =
      LazyList.unfold[A,RNG](rng)(rng => Some(g.run(rng)))

  // Exercise 5
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State { rng =>
      val (i, rng2) = rng.nextInt
      val i2 = start + (i & Int.MaxValue) % (stopExclusive - start)
      (i2, rng2)
    }

  // Exercise 6

  def unit[A] (a: =>A): Gen[A] =  
    State.unit(a)

  def boolean: Gen[Boolean] = 
    State { rng =>
      val (i, nextRng) = rng.nextInt
      (i % 2 == 0, nextRng)
    }


  def double: Gen[Double] = 
    State { rng =>
      val (i, nextRng) = rng.nextInt
      val d = i.toDouble / Int.MaxValue
      (i.toDouble / Int.MaxValue, nextRng)
    }
    

  // Exercise 7
  
  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] = 
      State { rng =>
        def go(count: Int, r: RNG, acc: List[A]): (List[A], RNG) = {
          if (count <= 0) (acc, r)
          else {
            val (a, r2) = self.run(r)
            go(count - 1, r2, a :: acc)
          }
        }
        go(n, rng, List.empty[A])
      }

  // Exercise 8
 
  // listOfN method was implemented as an extension method of Gen[A], was to leverage Scala's
  // extension methods feature, which allows adding new methods to existing types
  // withput modifying their original definitions. 
  
  // Exercise 9

  extension [A](self: Gen[A])

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State { rng =>
        val (a, rng2) = self.run(rng)
        f(a).run(rng2)
      }

    // It will be convenient to also have map (uses flatMap)
    def map[B](f: A => B): Gen[B] = 
      self.flatMap { a => unit(f(a)) }


  // Exercise 10

  extension [A](self: Gen[A])
    def listOf(size: Gen[Int]): Gen[List[A]] =
      size.flatMap { n => self.listOfN(n) }

  // Exercise 11

  extension [A](self: Gen[A])
    def union (that: Gen[A]): Gen[A] =
      boolean.flatMap { b => if b then self else that }

end Gen

import Gen.*

object Exercise_12:

  // Exercise 12 (type classes, givens, using, summon)
 
  // Choose one of the following templates, but note only listOfN and
  // listOf are tested (so rename if you use another than the first)

  def listOfN[A: Gen](n: Int): Gen[List[A]] =
    summon[Gen[A]].listOfN(n)
  
  def listOfN_[A: Gen]: Int => Gen[List[A]] =
    ???
  
  def listOfN__ [A](n: Int)(using genA: Gen[A]): Gen[List[A]] =
    ???
  // Choose one of the following templates, but note only listOfN and
  // listOf are tested (so rename if you use another than the first)
  
  def listOf[A: Gen](using genInt: Gen[Int]): Gen[List[A]] =
    summon[Gen[A]].listOf(genInt)
  
  def listOf_[A] (using genInt: Gen[Int], genA: Gen[A]): Gen[List[A]] =
    ???

end Exercise_12


// (Exercise 13)
// Read the code below (and/or Section 8.1.6). You will find the
// exercise in the bottom of this fragment, marked with ??? as usual.

opaque type TestCases = Int
opaque type FailedCase = String
opaque type SuccessCount = Int
opaque type MaxSize = Int

/** The type of results returned by property testing. */
enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true

end Result

import Result.{Passed, Falsified}

opaque type Prop = (MaxSize, TestCases, RNG) => Result

def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
  LazyList.unfold(rng)(rng => Some(g.run(rng)))

def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
  (max, n, rng) => 
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { (a, i) =>
             try if f(a) then Passed else Falsified(a.toString, i)
             catch case e: Exception => Falsified(buildMsg(a, e), i) }
      .find(_.isFalsified)
      .getOrElse(Passed)

def forAllNotSized[A] = forAll[A]
  
extension (self: Prop)
  infix def && (that: Prop): Prop = (maxSize, tcs, rng) => 
    self(maxSize, tcs, rng) match
      case Passed => that(maxSize, tcs, rng)
      case x => x
  
  infix def || (that: Prop): Prop = (maxSize, tcs, rng) =>
    self(maxSize, tcs, rng) match
      case Passed => Passed
      case _ => that(maxSize, tcs, rng)


// Exercise 14

/** The type of generators bounded by size */
opaque type SGen[+A] = Int => Gen[A]

extension [A](self: Gen[A])
  def unsized: SGen[A] = _ => self


// Exercise 15

extension [A](self: Gen[A]) 
  def list: SGen[List[A]] = 
    n => self.listOfN(n)

// A sized implementation of prop, takes MaxSize to generate
// test cases of given size.  
//
// The code below also contains the `run` method for Prop - which
// provides a simple way to execute tests. Needed in the final
// exercise below.

object SGen:

  object Prop:

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      (max, n, rng) =>
        val casesPerSize = (n.toInt - 1)/max.toInt + 1
        val props: LazyList[Prop] =
          LazyList.from(0)
            .take((n.toInt min max.toInt) + 1)
            .map { i => forAllNotSized(g(i))(f) }
        val prop: Prop =
          props.map[Prop](p => (max, n, rng) => 
            p(max, casesPerSize, rng)).toList.reduce(_ && _)
        prop(max, n, rng)

  extension (self: Prop)
    def run(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = RNG.Simple(System.currentTimeMillis)): Boolean =

      self(maxSize, testCases, rng) match
      case Result.Falsified(msg, n) =>
        println(s"\u001b[34m! Falsified after $n passed tests:\n $msg [message from our Prop framework]")
        false
      case Result.Passed =>
        println(s"\u001b[34m+ OK, passed $testCases tests. [message from our Prop framework]")
        true

end SGen

// Exercise 16

// Use this generator explicitly in the two properties
val nonEmptyList: SGen[List[Int]] =  
  (n: Int) => Gen.choose(-100, 100).listOfN(n max 1)

object Exercise_16:
  
  import SGen.*

  // The properties are put into a function taking `minimum` as argument to
  // allow the teachers to test them with different mutants of `minimum`.
  
  def p1Min(minimum: List[Int] => Int): Prop = 
    Prop.forAll(nonEmptyList) { list =>
      val min = minimum(list)
      list.forall(_ >= min)
    }

  def p2Min(minimum: List[Int] => Int): Prop = 
    Prop.forAll(nonEmptyList) { list =>
      val min = minimum(list)
      list.contains(min)
    }

end Exercise_16

// vim:cc=80:conceallevel=1
