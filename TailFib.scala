object Z2_1 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, a: Int, b: Int): Int = {
      if (n<=0) b
      else loop(n-1, b, a+b)
    }
    loop(n, 0, 1)
  }

  @annotation.tailrec
  def fib2(n: Int, a: Int = 0, b: Int = 1): Int = {
    if (n<=0) b
    else fib2(n-1, b, a+b)   
  }

  def main(args: Array[String]) = {
    println("tail fib(%d) = %d".format(10, fib(10)))
    println("tail fib2(%d) = %d".format(10, fib2(10)))
  }
}