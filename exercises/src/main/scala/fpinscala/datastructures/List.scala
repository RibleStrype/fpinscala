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
    case Cons(_, tail) => tail
    case Nil           => sys.error("tail of empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case Nil           => sys.error("setHead on empty list")
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {      case Nil           => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(head, tail)
      if f(head) => dropWhile(tail)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b) { (acc, a) =>
      b => acc(f(a, b))
    }(z)

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b) { (a, acc) =>
      b => acc(f(b, a))
    }(z)

  def append2[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)((x, ys) => Cons(x, ys))

  def append3[A](xs: List[A], ys: List[A]): List[A] =
    foldLeft(reverse(xs), ys)((ys, x) => Cons(x, ys))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def add1(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addLists(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Cons(x, tx), Cons(y, ty)) => Cons(x + y, addLists(tx, ty))
    case _ => Nil
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
      case _ => Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && startsWith(t1, t2)
    }

    @tailrec
    def go(sup: List[A]): Boolean =
      if (startsWith(sup, sub)) true
      else sup match {
        case Nil => false
        case Cons(_, t) => go(t)
      }

    go(sup)
  }
}
