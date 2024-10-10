// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)
  
/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 
/*
for LazyList01:
! testing.Ex01.01: headOption returns None on an empty LazyList: Exception raised on property evaluation.
+ testing.Ex01.02: headOption returns the head of the stream packaged in Some: OK, passed 100 tests.
for LazyList02:
+ testing.Ex01.01: headOption returns None on an empty LazyList: OK, proved property.
! testing.Ex01.02: headOption returns the head of the stream packaged in Some: Falsified after 0 passed tests.
*/
  // Exercise 2
  property("Ex02: headOption does not force the tail of a lazy list") = {
    var tailForced = false
    val lazyList = cons(1, { tailForced = true
      LazyList.empty })
    lazyList.headOption == Some(1) && !tailForced
  }

  // Exercise 3
  property("Ex03: take does not force any heads or tails") = {
    var headForced = false
    var tailForced = false
    val lazyList = LazyList.cons(
      { headForced = true; 1 }, 
      { tailForced = true; LazyList.empty }
    )
    val taken = lazyList.take(1)

    !headForced && !tailForced
  }

  // Exercise 4
  property("Ex04: take(n) does not force the (n+1)st head even if we force all elements of take(n)") =  {
    var forcedHeads = 0
    def makeList: LazyList[Int] = cons({ forcedHeads += 1; forcedHeads }, makeList)
    makeList.take(10).toList
    forcedHeads == 10
  }
  
  // Exercise 5
  property("Ex05: l.take(n).take(n) == l.take(n) for any lazy list l and any n") = 
    forAll(genNonEmptyLazyList[Int], Gen.choose(0, 100)) { (l, n) =>
    val firstTake = l.take(n).toList
    val secondTake = l.take(n).take(n).toList
    firstTake == secondTake
  }
  
  // Exercise 6
  property("Ex06: l.drop(n).drop(m) == l.drop(n+m) for any lazy list l, and any n and m") = 
    forAll(genNonEmptyLazyList[Int], Gen.choose(0, 100), Gen.choose(0, 100)) { (l, n, m) =>
    val firstDrop = l.drop(n).drop(m).toList
    val secondDrop = l.drop(n+m).toList
    firstDrop == secondDrop
  }
  
  // Exercise 7
  property("Ex07: l.drop(n) does not force any of the dropped elements") = 
    forAll(Gen.choose(0, 100)) { (n: Int) =>
    var forcedHeads = 0
    def makeList: LazyList[Int] = cons({ forcedHeads += 1; forcedHeads }, makeList)
    makeList.drop(n)
    forcedHeads == 0
  }

  // Exercise 8
  property("Ex08: l.map(identity) == l for any lazy list l") = 
    forAll(genNonEmptyLazyList[Int]) { (l: LazyList[Int]) =>
    l.map(identity).toList == l.toList
  }

  // Exercise 9
  property("Ex09: Map terminates on infinite lists") = 
    forAll(infiniteLazyList[Int]) { (l: LazyList[Int]) =>
    l.map(identity)
    true
  }
 
  // Exercise 10
  property("Ex10.01: append preserves length (length(l1.append(l2)) == length(l1) + length(l2))") = 
    forAll(genNonEmptyLazyList[Int], genNonEmptyLazyList[Int]) { (l1, l2) =>
    l1.append(l2).toList.length == l1.toList.length + l2.toList.length
  }

  property("Ex10.02: Left identity empty.append(l) == l") = 
    forAll(genNonEmptyLazyList[Int]) { (l: LazyList[Int]) =>
    LazyList.empty.append(l).toList == l.toList
  }

  property("Ex10.03: Right identity l.append(empty) == l") = 
    forAll(genNonEmptyLazyList[Int]) { (l: LazyList[Int]) =>
    l.append(LazyList.empty).toList == l.toList
  }

  property("Ex10.04: l1.append(l2) preserves element order") = 
    forAll(genNonEmptyLazyList[Int], genNonEmptyLazyList[Int]) { (l1, l2) =>
    l1.append(l2).toList == l1.toList ++ l2.toList
  }

  property("Ex10.05: append terminates when appending a finite list to an infinite list") = 
    forAll(genNonEmptyLazyList[Int]) { (l: LazyList[Int]) =>
    val infiniteList = LazyList.from(1)
    val appendedList = infiniteList.append(l).take(100).toList
    appendedList.length == 100
  }

