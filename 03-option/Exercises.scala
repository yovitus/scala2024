// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint 
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int = 
    if (this.x < that.x) -1
    else if (this.x > that.x) 1
    else if (this.y < that.y) -1
    else if (this.y > that.y) 1
    else 0



// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro._
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

import scala.math.Numeric.Implicits.infixNumericOps

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int =
    def go(t: Tree[A]): Int = t match
      case Leaf(_) => 1
      case Branch(l, r) => go(l) + go(r) + 1
    go(t)
    

  // Exercise 3

  def maximum(t: Tree[Int]): Int =
    def go(t: Tree[Int]): Int = t match
      case Leaf(v) => v
      case Branch(l, r) => go(l) max go(r)
    go(t)

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  

  // Exercise 5

  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match
    case Tree.Leaf(v) => g(v)
    case Tree.Branch(l, r) => f(fold(l)(f)(g), fold(r)(f)(g))

  def size1[A](t: Tree[A]): Int = 
    fold[A, Int](t)((l: Int, r: Int) => l + r + 1)(_ => 1)
  
  def maximum1(t: Tree[Int]): Int =
    fold[Int, Int](t)(_ max _)(identity)


  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(Branch(_,_))((a: A) => Leaf(f(a)))


enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] = this match
    case Some(value) => Some(f(value))
    case None => None 

  def getOrElse[B >: A] (default: => B): B = this match
    case Some(value) => value
    case None => default

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(value) => f(value)
    case None => None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case Some(value) => Some(value)
    case None => ob
  

  def filter(p: A => Boolean): Option[A] = this match
    case Some(value) if p(value) => Some(value)
    case _ => None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)
    



object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    ???

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    ???

  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    ???
    
end Option

 

// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] =
  ???

def headGrade1(lst: List[(String,Int)]): Option[Int] =
  ???

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
  ???
 
// Scroll up, to the Option object for Exercise 9
