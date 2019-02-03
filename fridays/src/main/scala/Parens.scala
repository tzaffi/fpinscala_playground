object Parens {
  def breakup(s: String, N: Int): (Seq[String], Int) = {
    val n = if (N>0) N else 1
    val len = s.length / n
    val pieces = (0 to (n-1))
    .map(i => s.substring(i*len, if (i < n-1) (i+1)*len else s.length) )
    (pieces, len)
  }

  def balanceR(p: String, N: Int): Either[String, Int] = {
    val b = breakup(p, N)
    val len = b._2
    b._1.map(parenCounter(_))
    .foldLeft( (Right(0), 0): (Either[String,Int], Int) )( (z,c) => z match {
      case (Left(_),i)  => (z._1, i+1)
      case (Right(x),i) => c match {
        case Left(j) => (Left("illegal char at index %d".format(i*len + j)), i+1)
        case Right(d) => {
          val m = d._1
          val b = d._2
          if (x+m < 0) (Left("unbalanced after index %d".format(i*len)), i+1)
          else (Right(x+b), i+1)
        }
      }
    })._1
  }

  //returns:
  // Left(i) - if has an illegal char and i is the index of the virst violation
  // Right((m, b)) - where m is the maximum imbalance depth
  //                 and b is the overall balance
  private def parenCounter(p: String): Either[Int, (Int,Int)] =   
    (0 to (p.length-1))
    .map(p.charAt(_))
    .foldLeft( (Right((0,0)), 0): (Either[Int, (Int,Int)], Int) )( (z,c) => z match {
      case (Left(_),i)  => (z._1, i+1)
      case (Right(x),i) =>
        if ( c != ')' && c != '(' ) (Left(i), i+1)
        else {
          val b = x._2 + (if (c == '(') 1 else -1)
          val m = x._1 min b
          (Right(m, b),i+1)
        }
    })._1

  // def balanceE(p: String): Either[String, Int] = parenCounter(p) match {
  //   case Left(x) => Left(x)
  //   case Right(x) =>
  //     if (x < 0) Left("underbalanced by %d".format(x))
  //     else Right(x)
  // }

  def balanceE(p: String): Either[String, Int] =
    (0 to (p.length-1))
    .map(p.charAt(_))
    .foldLeft( (Right(0), 0): (Either[String,Int], Int) )( (z,c) => z match {
      case (Left(_),i)  => (z._1, i+1)
      case (Right(x),i) =>
        if ( c != ')' && c != '(' )
          (Left("illegal char '%c' at index %d".format(c, i)), i+1)
        else if (x==0 && c != '(')
          (Left("unbalanced at index %d".format(i)), i+1)
        else if (c == '(')
          (Right(x+1), i+1)
        else
          (Right(x-1), i+1)
    })._1

  def balance(p: String): Int = {
    (0 to (p.length-1))
    .map(p.charAt(_))
    .foldLeft(0)( (z,c) =>
      if (z < 0 || ( c != ')' && c != '(' ))
        -1
      else if (c == '(')
        z+1
      else
        z-1
    )
  }
}
