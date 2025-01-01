file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/06-testing/Exercises.scala
### java.lang.AssertionError: assertion failed: denotation module class Mirror$ invalid in run 3. ValidFor: Period(4.1-5)

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/06-testing/Exercises.scala
text:
```scala
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


```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.core.Denotations$SingleDenotation.updateValidity(Denotations.scala:723)
	dotty.tools.dotc.core.Denotations$SingleDenotation.bringForward(Denotations.scala:748)
	dotty.tools.dotc.core.Denotations$SingleDenotation.toNewRun$1(Denotations.scala:805)
	dotty.tools.dotc.core.Denotations$SingleDenotation.current(Denotations.scala:876)
	dotty.tools.dotc.core.Symbols$Symbol.recomputeDenot(Symbols.scala:124)
	dotty.tools.dotc.core.Symbols$Symbol.computeDenot(Symbols.scala:118)
	dotty.tools.dotc.core.Symbols$Symbol.denot(Symbols.scala:109)
	dotty.tools.dotc.core.SymDenotations$.stillValidInOwner(SymDenotations.scala:2683)
	dotty.tools.dotc.core.SymDenotations$.stillValid(SymDenotations.scala:2679)
	dotty.tools.dotc.core.Denotations$SingleDenotation.bringForward(Denotations.scala:748)
	dotty.tools.dotc.core.Denotations$SingleDenotation.toNewRun$1(Denotations.scala:805)
	dotty.tools.dotc.core.Denotations$SingleDenotation.current(Denotations.scala:876)
	dotty.tools.dotc.core.Symbols$Symbol.recomputeDenot(Symbols.scala:124)
	dotty.tools.dotc.core.Symbols$Symbol.computeDenot(Symbols.scala:118)
	dotty.tools.dotc.core.Symbols$Symbol.denot(Symbols.scala:109)
	dotty.tools.dotc.core.Types$Type.baseType(Types.scala:1194)
	dotty.tools.dotc.typer.Synthesizer.baseWithRefinements$1(Synthesizer.scala:747)
	dotty.tools.dotc.typer.Synthesizer.recur$1(Synthesizer.scala:748)
	dotty.tools.dotc.typer.Synthesizer.recur$1$$anonfun$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer$.dotty$tools$dotc$typer$Synthesizer$$$orElse(Synthesizer.scala:773)
	dotty.tools.dotc.typer.Synthesizer.recur$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer.recur$1$$anonfun$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer$.dotty$tools$dotc$typer$Synthesizer$$$orElse(Synthesizer.scala:773)
	dotty.tools.dotc.typer.Synthesizer.recur$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer.recur$1$$anonfun$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer$.dotty$tools$dotc$typer$Synthesizer$$$orElse(Synthesizer.scala:773)
	dotty.tools.dotc.typer.Synthesizer.recur$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer.recur$1$$anonfun$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer$.dotty$tools$dotc$typer$Synthesizer$$$orElse(Synthesizer.scala:773)
	dotty.tools.dotc.typer.Synthesizer.recur$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer.recur$1$$anonfun$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer$.dotty$tools$dotc$typer$Synthesizer$$$orElse(Synthesizer.scala:773)
	dotty.tools.dotc.typer.Synthesizer.recur$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer.recur$1$$anonfun$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer$.dotty$tools$dotc$typer$Synthesizer$$$orElse(Synthesizer.scala:773)
	dotty.tools.dotc.typer.Synthesizer.recur$1(Synthesizer.scala:754)
	dotty.tools.dotc.typer.Synthesizer.tryAll(Synthesizer.scala:757)
	dotty.tools.dotc.typer.Implicits.inferImplicitArg(Implicits.scala:917)
	dotty.tools.dotc.typer.Implicits.inferImplicitArg$(Implicits.scala:845)
	dotty.tools.dotc.typer.Typer.inferImplicitArg(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.implicitArgs$1(Typer.scala:3839)
	dotty.tools.dotc.typer.Typer.addImplicitArgs$1(Typer.scala:3875)
	dotty.tools.dotc.typer.Typer.adaptNoArgsImplicitMethod$1(Typer.scala:3954)
	dotty.tools.dotc.typer.Typer.adaptNoArgs$1(Typer.scala:4156)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4407)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3701)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3413)
	dotty.tools.dotc.typer.Typer.typeSelectOnTerm$1(Typer.scala:775)
	dotty.tools.dotc.typer.Typer.typedSelect(Typer.scala:817)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3220)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3413)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:957)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3137)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3221)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3413)
	dotty.tools.dotc.typer.Typer.typeSelectOnTerm$1(Typer.scala:775)
	dotty.tools.dotc.typer.Typer.typedSelect(Typer.scala:817)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3112)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3220)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3413)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:957)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1117)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:351)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:120)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3137)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3221)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3221)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3413)
	dotty.tools.dotc.typer.Typer.$anonfun$63(Typer.scala:2627)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:256)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2627)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3119)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3220)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3324)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3370)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2814)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3125)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3129)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3220)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3324)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3370)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2947)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3171)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3221)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3298)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3302)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3413)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:47)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:477)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:53)
	dotty.tools.dotc.typer.TyperPhase.$anonfun$4(TyperPhase.scala:99)
	scala.collection.Iterator$$anon$6.hasNext(Iterator.scala:479)
	scala.collection.Iterator$$anon$9.hasNext(Iterator.scala:583)
	scala.collection.immutable.List.prependedAll(List.scala:152)
	scala.collection.immutable.List$.from(List.scala:684)
	scala.collection.immutable.List$.from(List.scala:681)
	scala.collection.IterableOps$WithFilter.map(Iterable.scala:898)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:100)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:315)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1323)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:337)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:350)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:360)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:69)
	dotty.tools.dotc.Run.compileUnits(Run.scala:360)
	dotty.tools.dotc.Run.compileSources(Run.scala:261)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:161)
	dotty.tools.pc.MetalsDriver.run(MetalsDriver.scala:47)
	dotty.tools.pc.PcCollector.<init>(PcCollector.scala:42)
	dotty.tools.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:88)
	dotty.tools.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:109)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: denotation module class Mirror$ invalid in run 3. ValidFor: Period(4.1-5)