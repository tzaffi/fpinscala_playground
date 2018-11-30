object Z_2_partial_lambda {
  def partial[A,B,C](a: A, f: (A,B) => C): B => C = {
    b => f(a, b)
  }
}

object Z_2_3 {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => (b => f(a, b))
  }
}

object Z_2_4 {
  def uncurry[A,B,C](f: A => (B => C)): (A,B) => C = {
    (a,b) => f(a)(b)
  }
}

object Z_2_5 {
  def compose[A,B,C](f: A => B, g: B => C): A => C = {
    //EQUIVALENT:       g compose f
    //DOES NOT COMPILE: f compose g
    a => g(f(a))
  }
}


