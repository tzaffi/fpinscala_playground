
import ZStream._
sealed trait ZStream[+A] {

  //Book's:
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case ZCons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }


  // this was a bad solution (non tail recursive):
  // def toList: List[A] = this match {
  //   case ZEmpty => Nil
  //   case ZCons(h,t) => h() +: t().toList
  // }

  // author's solution (build in reverse, then reverse at the end):
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: ZStream[A], acc: List[A]): List[A] = s match {
      case ZCons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  // def length(): Int = {
  // }

  def take(n: Int): ZStream[A] = this match {
    case ZEmpty => empty
    case ZCons(h,t) =>
      if (n<=0) empty
      else if (n == 1) cons(h(), empty)
      else cons(h(), t().take(n-1))
  }

  def takeWhile(p: A => Boolean): ZStream[A] = this match {
    case ZEmpty => empty
    case ZCons(h,t) => {
      lazy val head = h
      lazy val tail = t
      if (p(head())) cons(head(), tail() takeWhile(p))
      else empty
    }
  }

  def takeWhileUsingFoldRight(p: A => Boolean): ZStream[A] = {
    foldRight(empty[A])((h,t) =>
      if(p(h)) cons(h,t)
      else empty
    )
  }

  @annotation.tailrec
  final def drop(n: Int): ZStream[A] = this match {
    case ZEmpty => empty
    case ZCons(h,t) =>
      if (n<=0) this
      else t().drop(n-1)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case ZCons(h,t) =>
      if(p(h())) t().forAll(p)
      else false
    case _ => true      
  }

  def headOption(): Option[A] = {
    foldRight(None: Option[A])((h,_) => Some(h))
  }

  def map[B](f: A => B): ZStream[B] =
    foldRight(empty[B])((h, t) =>
      cons(f(h), t)
    )

  def filter(p: A => Boolean): ZStream[A] =
    foldRight(empty[A])((h,t) =>
      if (p(h)) cons(h, t)
      else t
    )

  def append[B>:A](right: => ZStream[B]): ZStream[B] =
    foldRight(right)((h,t) => cons(h,t))

  def flatMap[B](f: A => ZStream[B]): ZStream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

}
case object ZEmpty extends ZStream[Nothing]
case class ZCons[+A](h: () => A, t: () => ZStream[A]) extends ZStream[A]

object ZStream {
  def cons[A](hd: => A, tl: => ZStream[A]): ZStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    ZCons[A](() => head, () => tail)
  }
  def empty[A]: ZStream[A] = ZEmpty

  def apply[A](as: A*): ZStream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def lazyApply[A](as: (() =>A)*): ZStream[A] = {
    if (as.isEmpty) empty
    else ZCons(as.head, () => lazyApply(as.tail: _*))
  } 
}
