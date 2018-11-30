object Z_2_p25 {
  def partial[A,B,C](a: A, f: (A,B) => C): B => C = {
    def res(b: B): C = 
      f(a, b)

    res
  }

  def f_eg(a: Int, b:Int): Int =
    a + b
}