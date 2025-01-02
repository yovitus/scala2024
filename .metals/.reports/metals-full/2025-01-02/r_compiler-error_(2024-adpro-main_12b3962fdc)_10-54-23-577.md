file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/14-old-exams/2023/Exam.scala
### java.lang.AssertionError: assertion failed: denotation module class Game$ invalid in run 3. ValidFor: Period(4.1-5)

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/14-old-exams/2023/Exam.scala
text:
```scala
/* Final Exam: Advanced Programming, by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2023: 05 January 2024
 *
 * The exam consists of 9 questions to be solved within 4 hours.
 * Solve the tasks in the file 'Exam.scala' (this file).
 *
 * You can use all functions provided in the included files,  as well
 * as functions we implemented in the course (if the source is missing
 * in this folder, you can add it to this file, so that things compile)
 *
 * You can access any static written aids, also online, but you are
 * not allowed to communicate with anybody or with anything (bots).
 * Using GitHub copilot ChatGPT and similar large language models
 * during the exam is not allowed. By submitting you legally declare to
 * have solved the problems alone, without communicating with anybody.
 *
 * Do not modify this file in other ways than answering the questions
 * (adding imports is allowed). Do not reorder the answers, and do not
 * remove question numbers or comments from the file. 
 *
 * Submit this file and only this file to LearnIT. Do not convert to
 * any other format than .scala. Do not submit the entire zip archive.
 * The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of
 * ideas, the use of concepts, clarity, and style. We will use
 * undisclosed automatic tests during grading, but not to compute the
 * final grade, but to help us debug your code.
 *
 * We do require that the file compiles.  The directory has a project
 * setup so compilation with scala-cli shall work out-of-the-box.
 * If you cannot make a fragment compile, put your solution in a
 * comment, next to the three question marks. We will grade the
 * solutions in comments as well.
 *
 * Files that do not compile will automatically fail the exam. 
 *
 * The size of the exam has been adjusted from previous years so that
 * you have time to work with the compiler. We do not recommend to run
 * and test the code if you are pressed for time. It is a good idea to
 * run and test, if you have time. 
 *
 * Good luck! */

package adpro

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.TripleEquals.*

import adpro.laziness.LazyList
import adpro.state.*

object Streaming: 

  /* QUESTION 1 ######################################################
   * Study the following recursive function. Then implement the same
   * semantics using a fold.
   */

  def fViaRec (l: LazyList[Int]): Int = 
    def doRec (l: LazyList[Int], z: Int): Int = 
      l match 
        case LazyList.Cons(hd, tl) => 
          if hd() % 2 == 1 
          then doRec(tl(), z+1) 
          else doRec(tl(), z)
        case LazyList.Empty => z

    doRec (l,0)


  def fViaFold (l: LazyList[Int]): Int = 
    ???

end Streaming



object Parsing2023:

  import adpro.Parsing.*
  import adpro.parsing.Sliceable.*

  /* QUESTION 2 ######################################################
   * The following parser parses CSV (comma separated values) input
   * containing of integer numbers (and  no other types). The type of
   * values produced by a successful parse is List[List[Int]].
   *
   * Example input:
   * 1,2,3 , 5,4
   * 42,42
   *
   * Example output: 
   * List(List(1,2,3,5,4), List(42,42))
   *
   * Use this to write a parser `longestLine` of type Parser[Int] that
   * returns the maximum number of columns in the longest line in the
   * input, measured by the number of integers in the line. The parser
   * should produce Right(5) for the above example.
   */

  val WS: Parser[String] = regex("""[ \t]+""".r)
  val NL: Parser[String] = string("\n")
  val INT: Parser[Int] = 
    regex("""(\+|-)?[0-9]+""".r).map { _.toInt }

  val commaSeparatedInts: Parser[List[Int]] =
    { WS.? |* INT *| WS.? ** (string(",") |* WS.? |* INT *| WS.?).* }
      .map { (h,t) => h::t }

  lazy val parser: Parser[List[List[Int]]] =
    { commaSeparatedInts ** { ( NL |* commaSeparatedInts ) }.* }
      .map { (h,t) => h::t }

  lazy val longestLine: Parser[Int] = 
    ???


  /* QUESTION 3 ######################################################
   * Implement a parser of type Parser[Boolean] that parses a CSV
   * input and returns true if all lines have the same number of
   * elements.  It should return Right(False) for the above example.
   *
   * NB. This question does not require that you answered QUESTION 2.
   */

  val allLinesTheSame: Parser[Boolean] = 
    ???

end Parsing2023



object Game:

  import pigaro.*

  /* QUESTION 4 ######################################################
   * Consider the old Chinese game of rock, paper, and scissors.  Two
   * players independently and simultaneously select a move.  The
   * possible moves are Rock, Paper, and Scissors.  The winning is
   * established using the following rules:
   *
   * Rock wins against Scissors
   * Paper wins against Rock
   * Scissors win against Paper
   * Otherwise there is a draw.
   *
   * The function `winner` encodes the rules of the game, i.e. which
   * player wins in each pair of moves. `None` represents a draw.
   * Continue reading below.
   */

  enum Move:
    case Rock, Paper, Scissors

  enum Player: 
    case P1, P2

  import Move.*, Player.*

  type Result = Option[Player]

  def winner(player1: Move, player2: Move): Result = (player1, player2) match
    case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Some(P1)
    case (Scissors, Rock) | (Rock, Paper) | (Paper, Scissors) => Some(P2)
    case _ => None

  /* A strategy is a distribution over the next move, so Dist[Move].
   * Create two strategies, one for Alice and one for Bob:
   * a) Alice picks Rock, Paper, and Scissors with uniform probability.
   * b) Bob Picks Rock and Paper with probability 0.5 each, and never
   * picks Scissors.
   *
   * Link to relevant documentation 
   * https://www.itu.dk/people/wasowski/ula/scala-doc/index.html */

  type Strategy = Dist[Move]

  lazy val Alice: Strategy =
    ???

  lazy val Bob: Strategy =
    ???



  /* QUESTION 5 ######################################################
   * Implement a function that computes a result of a game given two
   * probabilistic strategies `player1` and `player2`. The function
   * shall give a probability of winning for each player, and a
   * probability of a draw as a Dist[Result].   You can use the
   * function 'winner' from above. 
   *
   * Answering QUESTION 4 is not required to answer this one.
   */
  def game (player1: Strategy, player2: Strategy): Dist[Result] =
    ???



  /* QUESTION 6 ######################################################
   * Obtain a sample of 10000 outcomes and  estimate the  probability
   * that Alice is winning when playing against  Bob.  You do not have
   * to report the number, just show the code that computes it.
   *
   * The correct answer will compile even if you leave QUESTIONS 4-5 
   * unanswered.
   */
  
  given rng: spire.random.rng.SecureJava 
    = spire.random.rng.SecureJava.apply

  lazy val aliceFraction: Double = 
    ???

end Game



object RL: 

  /* QUESTION 7 ######################################################
   * The type Q below represents Q-Tables in reinforcement learning,
   * so mapping states to actions to expected rewards. The function
   * update below produces a new q-table which has been modified for
   * the given state and action, using a new value of rewards and
   * estimate.  The details of how this update happens and how it is 
   * used in learning are not relevant for us here though.
   *
   * Our goal is to test the update function, and we will be testing
   * it on the State=Int, Action=Int case. We only test an update for
   * a state and action for which the Q-table q is defined.
   *
   * The type of the specialized update is written below as
   * `IntUpdate` for convenience.  Continue reading below.
   * */

  type Q[State, Action] = Map[State, Map[Action, Double]]

  val α = 0.5
  val γ = 1.0

  def update[State, Action](q: Q[State, Action], state: State, action: Action) 
    (reward: Double, estimate: Double): Q[State, Action] = 
    val qsa   = q(state)(action)
    val value = (1.0 - α) * qsa + α * (reward + γ * estimate) /* MARK */
    val av    = q(state) + (action -> value)
    q + (state -> av)

  type IntUpdate = 
    (Q[Int, Int], Int, Int) => (Double, Double) => Q[Int, Int]

  /* We need test data. We will test on q-tables initialized with zero
   * reward values for all actions and states.  The function qZero
   * below fills a Q-table of size nStates by nActions with zeroes.
   * States are counted from 0 to nStates-1 inclusively, and actions
   * are counted from 0 to nActions-1 inclusively.
   *
   * Continue reading below.
   */

  def qZero(nStates: Int, nActions: Int): Q[Int, Int] = 
    val actionMap: Map[Int, Double] = List.fill(nActions)(0.0)
        .zipWithIndex
        .map(_.swap)
        .toMap
    List.fill(nStates)(actionMap)
      .zipWithIndex
      .map(_.swap)
      .toMap

  /* We will also test on randomly initialized qTables, which are
   * created using the Scalacheck generator below.
   * Continue reading below.
   */

  val genRewards = Gen.choose(-100, 100).map(_.toDouble)
  def qGen(nStates: Int, nActions: Int): Gen[Q[Int, Int]] = for 
    av <- Gen.listOfN[Double](nActions, genRewards)
    av1 = av.zipWithIndex
            .map(_.swap)
            .toMap
    sav = List.fill(nStates)(av1)
              .zipWithIndex
              .map(_.swap)
              .toMap
  yield sav


  abstract class NullUpdatesSpec(update: IntUpdate, name: String) 
    extends org.scalacheck.Properties(name): 

    /* Write a **scenario** test that updates a zero-initialized 2 x 3
     * q-table (States in 0,1; Actions in 0,1,2) on the position
     * (0)(0) with arguments (reward=0.0, estimate=0.0). The test
     * should check that the obtained q-table is still the same as the
     * input table (such operation does nothing interesting).
     */

    property("00 Null update on null table 2x3") = 
      ???



    /* QUESTION 8 ####################################################
     * Write a **property** test that runs on randomly initialized
     * Q-Tables of size 2 x 3. The update should be performed on any
     * position (for any state, and any action within the range
     * defined in the Q-table), not only at (0)(0) as above. 
     *
     * The `reward` argument should get the value stored in the input
     * Q-table on the updated position. The estimate argument should
     * still be zero.
     *
     * Check that the resulting table under these conditions is the
     * same as the input table.
     */

    property("01 Null update on null table 2x3") = 
      ???

  end NullUpdatesSpec

  import monocle.*
  import monocle.syntax.all.*
  import monocle.function.all.*
  import monocle.function.Index
  import monocle.PLens.lensChoice

  /* QUESTION 9 ######################################################
   * We want to rewrite the update function using lenses.  The lens
   * `lens` below, implemented using the monocle library, projects a
   * q-table at its value at position (s,a) (i.e, state and action). 
   *
   * Reimplement the function 'update' to use this lens. Note that the
   * lens is used to obtain and replace the reward value. The actual
   * update calculation (marked MARK in the update function above)
   * will not change in this implementation, only the code to access
   * and replace the value.
   *
   * Assume that we only perform an update for a state and action for
   * which the Q table is defined.
   *
   * This question can be answered independently of the previous one.
   */

  def lens[S, A](s: S, a: A): Optional[Q[S,A], Double] = 
    val l1 = summon[Index[Q[S, A], S, Map[A, Double]]].index(s)
    val l2 = summon[Index[Map[A, Double], A, Double]].index(a)
    l1.andThen(l2)
  
  def updateWithLens[State, Action] (q: Q[State, Action], s: State, a: Action)
    (reward: Double, estimate: Double): Q[State, Action] =
    ???

end RL

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
	dotty.tools.dotc.core.Symbols$.toDenot(Symbols.scala:542)
	dotty.tools.dotc.core.TypeUtils.isTransparent(TypeUtils.scala:152)
	dotty.tools.dotc.core.ConstraintHandling.dropOneTransparentTrait$1(ConstraintHandling.scala:581)
	dotty.tools.dotc.core.ConstraintHandling.dropOneTransparentTrait$1(ConstraintHandling.scala:586)
	dotty.tools.dotc.core.ConstraintHandling.recur$1(ConstraintHandling.scala:596)
	dotty.tools.dotc.core.ConstraintHandling.dropTransparentTraits(ConstraintHandling.scala:605)
	dotty.tools.dotc.core.ConstraintHandling.dropTransparentTraits$(ConstraintHandling.scala:29)
	dotty.tools.dotc.core.TypeComparer.dropTransparentTraits(TypeComparer.scala:30)
	dotty.tools.dotc.core.ConstraintHandling.widenInferred(ConstraintHandling.scala:679)
	dotty.tools.dotc.core.ConstraintHandling.widenInferred$(ConstraintHandling.scala:29)
	dotty.tools.dotc.core.TypeComparer.widenInferred(TypeComparer.scala:30)
	dotty.tools.dotc.core.TypeComparer$.widenInferred(TypeComparer.scala:3275)
	dotty.tools.dotc.typer.Namer.rhsType$1(Namer.scala:1966)
	dotty.tools.dotc.typer.Namer.cookedRhsType$1(Namer.scala:1972)
	dotty.tools.dotc.typer.Namer.lhsType$1(Namer.scala:1973)
	dotty.tools.dotc.typer.Namer.inferredResultType(Namer.scala:1984)
	dotty.tools.dotc.typer.Namer.inferredType$1(Namer.scala:1728)
	dotty.tools.dotc.typer.Namer.valOrDefDefSig(Namer.scala:1734)
	dotty.tools.dotc.typer.Namer.defDefSig(Namer.scala:1833)
	dotty.tools.dotc.typer.Namer$Completer.typeSig(Namer.scala:808)
	dotty.tools.dotc.typer.Namer$Completer.completeInCreationContext(Namer.scala:955)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:831)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:178)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:190)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:192)
	dotty.tools.dotc.core.Types$TermRef.underlying(Types.scala:2954)
	dotty.tools.dotc.core.Types$Type.widen(Types.scala:1286)
	dotty.tools.dotc.typer.Typer.simplify(Typer.scala:3236)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3224)
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
	dotty.tools.dotc.typer.Typer.typedValDef(Typer.scala:2564)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3116)
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

java.lang.AssertionError: assertion failed: denotation module class Game$ invalid in run 3. ValidFor: Period(4.1-5)