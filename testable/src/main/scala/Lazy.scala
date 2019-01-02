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

  def takeViaUnfold(n: Int): ZStream[A] =
    unfold((this, n)) {
      case (ZCons(h,t), 0) => None
      case (ZCons(h,t), n) => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): ZStream[A] =
    unfold(this) {
      case ZCons(h,t) => if (p(h())) Some((h(), t())) else None
      case _ => None
    }

  def mapViaUnfold[B](f: A => B): ZStream[B] =
    unfold(this) {
      case ZCons(h,t) => Some((f(h()), t()))
      case _ => None      
    }

  def zipWith[B,C](right: ZStream[B])(f: (A,B) => C): ZStream[C] =
    unfold((this, right)) {
      case (ZCons(hl,tl),ZCons(hr,tr)) => Some((f(hl(),hr()), (tl(),tr())))
      case _ => None      
    }

  def zipAll[B](right: ZStream[B]): ZStream[(Option[A], Option[B])] = 
    unfold((this, right)) {
      case (ZCons(hl,tl),ZCons(hr,tr)) => Some((Some(hl()), Some(hr())), (tl(),tr()))
      case (ZCons(hl,tl),ZEmpty) =>  Some( (Some(hl()), None), (tl(), ZEmpty) )
      case (ZEmpty, ZCons(hr,tr)) => Some( (None, Some(hr())), (ZEmpty, tr()) )        
      case _ => None
    }
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

  def constant[A](a: A): ZStream[A] = cons(a, constant(a))

  def from(x: Int): ZStream[Int] = cons(x, from(x+1))

  private def fibs_impl(curr: Int, next: Int): ZStream[Int] =
    cons(curr, fibs_impl(next, curr+next))

  val fibs: ZStream[Int] = fibs_impl(0,1)

  def unfold[S, A](z: S)(f: S => Option[(A,S)]): ZStream[A] = {
    f(z) match {
      case None => empty
      case Some((a,s)) => cons(a, unfold(s)(f))
    }
  }

  val onesUsingUnfold = unfold(0)(_=>Option(1,0))

  def constantViaUnfold[A](a: A): ZStream[A] = unfold(0)(_=>Some(a,0))

  def fromViaUnfold(x: Int): ZStream[Int] = unfold(x)(s=>Some((s,s+1)))

  def fibsViaUnfold: ZStream[Int] = unfold((0,1))(s => Some((s._1, (s._2, s._1+s._2))))
}
