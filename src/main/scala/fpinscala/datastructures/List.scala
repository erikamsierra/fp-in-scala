sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("can't do tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => sys.error("can't set head of empty list")
    case Cons(_, t) => Cons(x, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("can't do init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sumFoldRight(as: List[Int]) =
    foldRight(as, 0)(_ + _) //(x,y) => x + y same as _+_

  def productFoldRight(as: List[Double]) =
    foldRight(as, 1.0)(_ * _) //(x,y) => x * y same as _*_

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumFoldLeft(as: List[Int]) =
    foldLeft(as, 0)(_ + _)

  def productFoldLeft(as: List[Double]) =
    foldLeft(as, 1.0)(_ * _)

  def lengthFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((h, t) => Cons(t,h))

//  // TODO
//  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
//
//  // TODO
//  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =

  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_)) //(x,y) => Cons(x, y) same as Cons(_,_)

  def concatenate[A](as: List[List[A]]): List[A] =
    foldRight(as, List[A]())(append) //(x,y) => append(x, y) same as append

  def add1(as: List[Int]): List[Int] =
    foldRight(as, List[Int]())((h,t) => Cons(h+1,t))

  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, List[String]())((h,t) => Cons(h.toString,t))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((h,t) => Cons(f(h),t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((h,t) => if (f(h)) Cons(h,t) else t)

}
