file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/10-prob/Exercises.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 8230
uri: file://<HOME>/Documents/Datalogi/1.%20semester/Advanced%20Programming/Repo/2024-adpro-main/10-prob/Exercises.scala
text:
```scala
// Advanced Programming, Andrzej Wasowski
// Probabilistic Programming (AKA Probability is also a monad)
package adpro.prob

import pigaro.*

// Used by samplers
given rng: spire.random.rng.SecureJava 
  = spire.random.rng.SecureJava.apply

// All the exercise text is included below in comments (no PDF file
// this week).

// Probabilistic programs are hard to test, so the tests we have are a
// bit weak. Also note, that more exercises you solve the slower the
// test suite becomes, due to many samplings it runs. You may want to
// disable tests temporarily to speed up work.

val M = 42

// For many exercises, you are (quietely) expected to run the queries
// in the REPL in order to inspect probability values (or to print
// them to the standard output).
//
// Hand in the completed file (this file, Exercises.scala). No zip
// files, no pdfs, etc.
//
// The main inspiration for this exercise comes from the material of a
// probabilistic programming course by prof. Joost-Pieter Katoen at
// RWTH Aachen, Germany.

// Before starting to solve the exercises below, please study the file
// Basic.sc, side-by-side with this week's slides. Once you
// understood the code in Basic.sc, come back here.

// Peter and Paula play a game.  An urn contains some black balls and
// a single red ball. They take turns taking one random ball out of
// the urn. The first one to pick the red ball wins.
//
// We model the players using, for instance, an enum:

enum Player:
  case Peter, Paula 

import Player.*

// And we add a function to determine the next player (a simple alternation):

def next(player: Player): Player = player match
  case Peter => Paula
  case Paula => Peter

// The number of balls, including exactly 1 red ball, found in the run at the
// begining of the game

val BallsNo: Int = 8

// Exercise 1.
//
// Write a function pick that given the number 'n' of black balls in
// the urn returns a probability distribution that generates Red with
// the probability of picking the red ball. There is always one red
// inside the urn, when a move is made.  Use the following constructor
// 'Pigaro.bernoulli' to implement 'pick'.

// bernoulli[U] (probability: Double, success: U, failure: U): Dist[U]

enum Ball: 
  case Red, Black
import Ball.*

def pick(n: Int): Dist[Ball] = {
  val redProbability = 1.0 / (n + 1.0)
  Pigaro.bernoulli(redProbability, Ball.Red, Ball.Black)
}
  

//  Exercise 2. 
//
//  Write a function 'move' that given the starting player and the
//  number of black balls 'n' present in the urn returns the
//  probability distribution defining which player wins. 
//
// Hint: Andrzej's solution used 'pick' and 'Dirac'. 
// 
// Dirac[A](a: A): Dist[A]
//
// This constructor returns a distribution where the value 'a' has
// probability '1'.  Dirac is basically a constant probability
// distribution - only one outcome is legal. This is the monadit
// const/unit/pure.


def move(player: Player, n: Int): Dist[Player] = 
  player match
    case Peter => pick(n).flatMap {
      case Red => Dirac(Peter)
      case Black => move(next(player), n-1)
    }
    case Paula => pick(n).flatMap {
      case Red => Dirac(Paula)
      case Black => move(next(player), n-1)
    }

// Exercise 3.
//
// Peter is polite and offers a choice to Paula, if she wants to start or
// rather had that he started.
//
// Use the function 'move' to estimate the chance of Paula winning
// when she starts, and when Peter starts, if the urn contains 'BallsNo' balls
// in total (including one red).   
//
// To calculate probability, first use d.sample(N) for some object of
// type Dist[...] to obtain a sample of N elements (type IData), and
// then use IData's method .pr to calculate the probability.  The
// latter takes a value from the distribution range. It returns the
// estimate of probability that the distribution takes this value.
//
// IData[A].pr(value: A): Double

// Probability that Paula wins given Paula starts (the total no of balls: BallsNo)
def probPaulaStarts: Double = 
  move(Paula, BallsNo - 1).sample(4000).pr(Paula)

// Probability that Paula wins given Peter starts (the total no of balls: BallsNo)
def probPeterStarts: Double = 
  move(Peter, BallsNo - 1).sample(4000).pr(Paula)


//  Which strategy is beter for Paula? What if BallsNo == 9? 
//  Write your answer here in a comment: With and odd number of balls, Paula has a higher chance of winning if she starts.
// With an even number of balls, the chances are equal.

// Exercise 4.
//
// A quick pen-and-pencil question: Can you estimate the size of
// the Bayesian network generated by 'move (p, 10)' for some player constant p?
//
// Observe, that this model would be very annoying and laborious to build
// manually on paper, but with statistical interpretation in a programming
// framework we can build models for 200 balls easily.  This is probably the
// main strength of probabilistic programming.
//
// You do not need to write the answer to this question for grading.
// Use it yourself to appreciate the power of the probabilistic programming
// tool).

// Exercise 5.

// We know that Paula has won the game.  What is the probability that she has
// started the game?  Use MAP (maximum posterior probability), assuming that
// it was initially equally likely that Peter and Paula are starting.
//
// This exercise is split in a number of smaller steps.  You should try to get
// an overview of the entire constructed model.
//
// We first create a uniform prior for the first mover:

lazy val firstMover: Dist[Player] =
  Pigaro.uniform("firstMover")(Player.Peter, Player.Paula)
  

// Now create a nullary function 'gameResult' that picks the first mover
// randomly using 'firstMover' and then returns the probability distribution
// for a game played with BallsNo balls in the urn. We want to keep
// both the first mover and the winner in the model, so that we can
// reason about who has started under the condition of who has won.
// Thus the type Dist[(Player, Player)] seems appropriate.
//
// The _flatMap function, or its domain-specific synonym probDep, may
// prove useful. 

def gameResult: Dist[(Player, Player)] = firstMover.flatMap { starter =>
  move(starter, BallsNo - 1).map { winner => (starter, winner) }
}
  

// What is the probability that Paula wins with this uniform prior? Does it
// agree with your intuition? Write the answer in a comment:
// ____

// Now we are going to make the observation that Paula wins. 

lazy val gameWonByPaula: Dist[(Player, Player)] = 
  gameResult.matching { case (_,Paula) => }

// Calculate the probability that Paula started given that she won.
// You will need to sample and use IData's .pr or .prMatching
// methods.

lazy val probPaulaStarted: Double = 
  gameWonByPaula.sample(4000).prMatching { case (Paula, _) => }

// Does this probability depend on the number of balls in the urn in the
// urn being even or odd? What if it is even? What if it is odd?
//
// ____



// Exercise 6.
//
// We know that winning player wins approximately 1/2 games when she
// starts, and we know now that if there is an even number of balls
// in the urn then the probability is precisely equal for both players, while
// if the number of balls is odd the probability of the first player winning
// is slightly higher.
//
// In this exercise, we assume that the number of balls is unknown, but it is
// taken from range 1 to 6 with uniform probability (uniform prior) and we
// will observe that Player1 has won.  We will ask what is the probability
// that the urn held an odd number of balls in the beginning of the game.  We
// expect this probability to be slightly higher than 50%, as player 1 winning
// makes as believe slightly that an odd number of balls are in the urn.

// Let UpperBound will be the maximum number of balls in the urn that we
// consider.

val UpperBound = 6

// Construct a uniform prior on the number of black balls in the urn
// from zero to UpperBound - 1.

// Use the Pigaro.uniform[A] constructor (a variadic function that takes all the
// equally like values of A as its variable size argument list):

// Pigaro.uniform[A](name: String)(a : A*) :Element[A]

lazy val blackBallsNo: Dist[Int] =
  Pigaro.uniform("blackBallsNo")(@@)

// Now convert the prior distribution on the initial number of black balls in
// the urn, into a distribution over the winning player.  Since the game is
// entirely symmetric, we can assume that Paula is starting (the result for
// Peter will be the same). Hint: flatMap or probDep

// There is no test for this step of the computation.

def outcome: Dist[(Int, Player)] = 
  ???

// The following asserts that Paula has won.

lazy val paulaWon: Dist[(Int, Player)] = 
  ???

// Now define the posterior probabilities for all size of the urn from 1 to
// UpperBound. You can do this using IData.pr.
// We need a slightly different version of the former that takes a predicate,
// not a concrete value (or the pattern match version):

// IData[T].pr (p: T => Boolean)
// IData[T].prMatching  { case ... => }

lazy val posteriorOdd: Double =
  ???

// Is the posteriorOdd greater than 1/2? Why?
//
// _____


// Reflect whether the above estimation would take you more time analytically
// or with a probabilistic programming library?


```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:243)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:53)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:409)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1