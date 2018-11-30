//package fpinscala.datastructures

// trait -  `List` data type, parameterized on a type, `A`:
// case object - A `List` data constructor representing the empty list:
// case class - Cons for non-empty lists:
sealed trait ZList[+A] 
case object Nil extends ZList[Nothing] 
case class Cons[+A](head: A, tail: ZList[A]) extends ZList[A]

object ZList { // `ZList` Contains functions for creating and working with lists:

  // A function that uses pattern matching to add up a list of integers:
  def sum(ints: ZList[Int]): Int = ints match { 
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // non-empty: `x` plus the sum of the rest of the list.
  }

  def product(ds: ZList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): ZList[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*** EXERCISES ***/

  //3.1: is x == 3? YES IT IS
  val x = ZList(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  val solved_3_1 = (x==3)

  //3.2:
  def tail[A](l: ZList[A]): ZList[A] = l match {
    case Nil => sys.error("tail of empty list is undefined")
    case Cons(h, t) => t
  } 

  //3.3:
  def setHead[A](l: ZList[A], h: A): ZList[A] = 
    Cons(h, tail[A](l))

  //3.4
  @annotation.tailrec
  def drop[A](l: ZList[A], n: Int): ZList[A] =
    if (n<=0) l // no-op
    else drop(tail(l), n-1)

  //3.5
  private def head_n_tail[A](l: ZList[A]): (A, ZList[A]) = l match {
    case Nil => sys.error("tail/head of empty list is undefined")
    case Cons(h, t) => (h, t)
  } 

  @annotation.tailrec
  def dropWhile[A](l: ZList[A], f: A => Boolean): ZList[A] = l match {
    case Nil => l
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }

/*
dropWhile(List(1,2,3), true)
dropWhile(List(2,3), true)
dropWhile(List(3), true)
dropWhile(Nil, true)
Nil
*/

  // 3.6- O(n) unfortunately:
 // @annotation.tailrec
  def init[A](l: ZList[A]): ZList[A] = l match {
    case Nil => sys.error("init of empty list is undefined")
    case Cons(first, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }

/**
init(List(1,2,3))
Cons(1,init(List(2,3)))
Cons(1,Cons(2,init(List(3))))
Cons(1,Cons(2,Nil))
**/

  // 3.7: nope, can't shortcut product

  def foldRight[A,B](as: ZList[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // 3.8 what happens when you pass Nil and Cons to foldRigh?
  // ANS: It should just construct the list. Prove it:
  val ans_3_8 = foldRight(ZList(1,2,3), Nil:ZList[Int])(Cons(_,_))

  // 3.9: length using foldRight
  def length[A](l: ZList[A]): Int =
    foldRight(l, 0)((x,y)=>1+y)

  // 3.10
  def foldLeft[A,B](l: ZList[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(as: ZList[A], res: B): B = as match {
      case Nil => res
      case Cons(x, xs) => loop(xs, f(res, x))
    }
    loop(l, z)
  }

  // 3.11 sum/prod/length using foldRight
  def sumLeft(ints: ZList[Int]): Int =
    foldLeft(ints, 0)(_+_)

  def prodLeft(ds: ZList[Double]): Double =
    foldLeft(ds, 1.0)(_*_)

  def lenLeft(ints: ZList[Int]): Int =
    foldLeft(ints, 0)( (acc,_) => acc+1)

  // 3.12 reverse using foldLeft
  def reverse[A](l: ZList[A]): ZList[A] = 
    foldLeft(l, Nil:ZList[A])((b,a)=>Cons(a,b))

  // 3.13 [H] foldRight in terms of foldLeft
  // my solution is MUCH simpler than the official answer
  def foldRightWithLeft[A,B](as: ZList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a)=>f(a,b))

  // 3.14 append using foldLeft
  def append[A](l1: ZList[A], l2: ZList[A]): ZList[A] =
    foldLeft(reverse(l1), l2)((xs,x)=>Cons(x,xs))

  // 3.15 [H] append a list of lists in O(n) time
  def appendLists[A](ll: ZList[ZList[A]]): ZList[A] =
    foldLeft(ll, Nil:ZList[A])(append)

  // 3.16 1 adder
  def addOne(ds: ZList[Double]): ZList[Double] =
    foldRightWithLeft(ds, Nil:ZList[Double])((d,acc) => Cons(d+1,acc))


  // 3.17 doubles -> strings
  def toStrings(ds: ZList[Double]): ZList[String] =
    foldRightWithLeft(ds, Nil:ZList[String])((d,acc) => Cons(d.toString, acc)) 

// 3.18 map
  def map[A,B](l: ZList[A])(f: A=>B): ZList[B] =
    foldRightWithLeft(l, ZList[B]())((a,b)=>Cons(f(a), b))

  // 3.19 filter - and use it to remove odd numbers from a list
  def filter[A,B](l: ZList[A])(f: A=>Boolean): ZList[A] =
    foldRightWithLeft(l, Nil:ZList[A])((x,acc) => if (f(x)) Cons(x, acc) else acc )

  val beforeFilter = ZList(-5,-4,-3,-2,-1,0,1,2,3,4,5)
  val afterFilter = filter(beforeFilter)(_%2 == 0)

  // 3.20 flatMap
  def flatMap[A,B](l: ZList[A])(f: A=>ZList[A]): ZList[A] =
    appendLists(map(l)(f))

  // 3.21 implement filter using flatmap:
  def filterUsingFlatmap[A,B](l: ZList[A])(f: A=>Boolean): ZList[A] =
    flatMap(l)(x => if (f(x)) ZList(x) else ZList())

  //3.22 implement zipAdder
  def zipAdder(z1s: ZList[Int], z2s: ZList[Int]): ZList[Int] = {
    @annotation.tailrec
    def loop(x1s: ZList[Int], x2s: ZList[Int], res: ZList[Int]): ZList[Int] = (x1s, x2s) match {
      case (Nil, Nil) => res
      case (Nil, _) => sys.error("first list is too short")
      case (_, Nil) => sys.error("second list is too short")
      case (Cons(y1, y1s), Cons(y2, y2s)) => loop(y1s, y2s, Cons(y1+y2, res))
    }
    loop(reverse(z1s), reverse(z2s), Nil)
  }


  //3.23 zipper
  def zipWith[A,B](z1s: ZList[A], z2s: ZList[A])(f: (A,A)=>B): ZList[B] = {
    @annotation.tailrec
    def loop(x1s: ZList[A], x2s: ZList[A], res: ZList[B]): ZList[B] = (x1s, x2s) match {
      case (Nil, Nil) => res
      case (Nil, _) => sys.error("first list is too short")
      case (_, Nil) => sys.error("second list is too short")
      case (Cons(y1, y1s), Cons(y2, y2s)) => loop(y1s, y2s, Cons(f(y1,y2), res))
    }
    reverse(loop(z1s, z2s, Nil))
  }

  //3.24 has subsequence using official list
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def loop(supRight: List[A], alreadyFound: Boolean): Boolean = supRight match {
      case List() => alreadyFound
      case _::t  => loop(t, alreadyFound || isPrefix(sub, t))
    }
    loop(sup, false)
  }

  private def isPrefix[A](sub: List[A], l: List[A]): Boolean = {
    @annotation.tailrec
    def loop(y: List[A], yPrefix: List[A], stillGood: Boolean): Boolean = (y, yPrefix) match {
      case (_, List()) => stillGood
      case (List(), _) => false
      case (hy::ty, hYp::tYp) => loop(ty, tYp, stillGood && hy==hYp)
    }

    loop(l, sub, true)
  }

}
