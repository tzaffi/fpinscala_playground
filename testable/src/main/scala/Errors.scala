sealed trait ZOption[+A] {
  def map[B](f: A => B): ZOption[B] = this match {
    case ZNone => ZNone
    case ZSome(x) => ZSome(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case ZNone => default
    case ZSome(x) => x
  }
  
  def flatMap[B](f: A => ZOption[B]): ZOption[B] =
    map(f) getOrElse ZNone

  def orElse[B >: A](ob: => ZOption[B]): ZOption[B] =
    this map(ZSome(_)) getOrElse(ob)

  def filter(f: A => Boolean): ZOption[A] = {
    val opt = this map(f)
    if (opt == ZNone || opt == ZSome(false))
      ZNone
    else
      this
  }
}
case class ZSome[+A](get: A) extends ZOption[A]
case object ZNone extends ZOption[Nothing]

object ZOption {
  def mean(xs: Seq[Double]): ZOption[Double] =
    if (xs.isEmpty)
      ZNone
    else
      ZSome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): ZOption[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => Math.pow(x-m, 2))))
  }

  def map2[A, B, C](x: ZOption[A], y: ZOption[B])(f: (A, B) => C): ZOption[C] = (x,y) match {
    case (ZSome(a), ZSome(b)) => ZSome(f(a,b))
    case _ => ZNone
  }

  def sequence[A](xs: List[ZOption[A]]): ZOption[List[A]] = {
    // ORIGINAL:
    // val empty: ZOption[List[A]] = ZSome(List())
    // xs.foldLeft(empty)((ol,oa) => map2[List[A], A, List[A]](ol,oa)((x: List[A], y: A) => x:+y))

    // EXPERIMENTAL WORKS TOO:
    // val empty: ZOption[List[A]] = ZSome(List())
    // xs.foldLeft(empty)((ol,oa) => map2[List[A], A, List[A]](ol,oa)(_:+_))

    // EXPERIMENTAL 2 WORKS TOO:
    // val empty: ZOption[List[A]] = ZSome(List())
    // xs.foldLeft(empty)(map2[List[A], A, List[A]](_,_)(_:+_))

    // ALMOST FINAL:
    //xs.foldLeft(ZSome(List()): ZOption[List[A]])(map2[List[A], A, List[A]](_,_)(_:+_))

    // FINAL FORM:
    xs.foldLeft(ZSome(List()): ZOption[List[A]])(map2(_,_)(_:+_))
  }

  def Try[A](a: => A): ZOption[A] =
    try ZSome(a)
    catch { case e: Exception => ZNone }

  def traverseWithTry[A, B](xs: List[A])(f: A => B): ZOption[List[B]] = {
    def folder(ol: ZOption[List[B]], a: A): ZOption[List[B]] = 
      if (ol == ZNone) ZNone
      else Try( ol.getOrElse(Nil) :+ f(a) )

    xs.foldLeft(ZSome(List()): ZOption[List[B]])(folder)
  }

  def traverse[A, B](xs: List[A])(f: A => ZOption[B]): ZOption[List[B]] = {
    //Not actually better:
    // val append  = (x: List[B], y: B) => x:+y
    // xs.foldLeft(ZSome(List()): ZOption[List[B]])((olb, a) => map2(olb,f(a))(append))

    // best I could get:
    xs.foldLeft(ZSome(List()): ZOption[List[B]])((olb, a) => map2(olb,f(a))(_:+_))
  }

  // f: ZOption[A] (=A') => ZOption[A] (=ZOption[B']) //so: A' = ZOption[A], B' = A
  def sequenceUsingTraverse[A](xs: List[ZOption[A]]): ZOption[List[A]] = {
    traverse(xs)(x=>x)
  }

  def lift[A, B](f: A => B): ZOption[A] => ZOption[B] = _.map(f)
}


sealed trait ZEither[+E, +A] {
  // private def getOrElse[B >: A](default: => B): B = this match {
  //   case ZLeft(e) => default
  //   case ZRight(a) => a
  // }
  // private def getErrElse[EE >: E](default: => EE): EE = this match {
  //   case ZLeft(e) => e
  //   case ZRight(a) => default
  // }

  def map[B](f: A => B): ZEither[E,B] = this match {
    case ZLeft(e) => ZLeft(e)
    case ZRight(a) => ZRight(f(a))
  }

  def flatMap[EE >: E, B](f: A => ZEither[EE,B]): ZEither[EE,B] = this match {
    case ZLeft(e) => ZLeft(e)
    case ZRight(a) => f(a)
  } 

  private def swap(): ZEither[A,E] = this match {
    case ZLeft(e) => ZRight(e)
    case ZRight(a) => ZLeft(a)
  }

  def orElse[EE >: E, B >: A](eb: => ZEither[EE,B]): ZEither[EE,B] =
    swap flatMap(x => eb swap) swap

  def map2[EE >: E, B, C](eb: ZEither[EE, B])(f: (A, B) => C): ZEither[EE, C] =
    for {
      a <- this
      b <- eb
    } yield f(a,b)

}
case class ZLeft[+E](error: E) extends ZEither[E, Nothing]
case class ZRight[+A](value: A) extends ZEither[Nothing, A]


object ZEither {
  def Try[A](x: => A): ZEither[String, A] =
    try ZRight(x)
    catch { case e: Exception => ZLeft(e.toString) }

  def traverse[E,A,B](xs: List[A])(f: A=>ZEither[E,B]): ZEither[E,List[B]] =
    xs.foldLeft[ZEither[E,List[B]]](ZRight(Nil))((elb, a) => elb.map2(f(a))( _ :+ _ ))

  def sequence[E, A](xs: List[ZEither[E,A]]): ZEither[E,List[A]] =
   traverse(xs)(x=>x)
}




