// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition
package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n * n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    println(square(magic))
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))

end MyModule

// Exercise 2 requires no programming

// Exercise 3
def fib(n: Int): Int =
  @annotation.tailrec
  def f(n: Int, acc1: Int, acc2: Int): Int =
      if n <= 0 then acc1
      else f(n - 1, acc2, acc1 + acc2)
  f(n, 0, 1)

// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
  @annotation.tailrec
  def help(n: Int): Boolean = {
    if (n >= as.length - 1) then true
    else if (!ordered(as(n), as(n + 1))) then false
    else help(n + 1)
  }
  help(0)


// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  A => B => f(A, B)


def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  curry(isSorted)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (A, B) => f(A)(B)

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  uncurry(isSortedCurried)

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  A => f(g(A))
