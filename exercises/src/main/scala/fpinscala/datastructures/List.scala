package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => if (n > 0) drop(xs, n - 1) else Cons(x, xs)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(list: List[B => B], acc: B): B = {
      list match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, x(acc))
      }
    }

    loop(foldRight(l, Nil: List[B => B])((e, acc) => Cons((a) => f(a, e), acc)), z)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b: B) => b)((a, acc) => b => acc(f(b, a)))(z)

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, (b: B) => b)((acc, e) => b => acc(f(e, b)))(z)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  val y = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def appendFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r)((x, y) => Cons(y, x))

  def concatenate[A](l: List[List[A]]): List[A] = reverse(foldLeft(l, Nil: List[A])((acc, e) => foldLeft(e, acc)((x, y) => Cons(y, x))))

  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, y) => Cons(x.toString, y))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(reverse(as), Nil:List[B])((e, acc) => appendFoldLeft(acc, f(e)))

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = concatenate(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else List())

  def zipWith[A,B,C](l:List[A], r:List[B])(f:(A,B) => C):List[C] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  val z = zipWith(List(1,2,3), List(4,5))((x, y) => x + y)

}
