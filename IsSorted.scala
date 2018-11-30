object Z2_2 {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)
    }
    
    loop(0)
  }

  private def intTestCases(): Array[(Array[Int], Boolean)] = {
    Array(
      (Array(1, 2, 3), true),
      (Array(1, 3, 2), false),
      (Array(1), true),
      (Array(), true),
      (Array(1, 2, 3, -4, 7), false),
      (Array(1, 2, 3, 7, 11, 20, 40, 40, 41), true),      
    )
  }

  private def strTestCases(): Array[(Array[String], Boolean)] = {
    Array(
      (Array("1", "2", "3"), true),
      (Array("one", "two", "three"), false),
    )
  } 
  

  private def evalTestCase[A](as: Array[A], ordered: (A,A) => Boolean, expect: Boolean): String = {
    val res = isSorted(as, ordered)
    val msg = "%s =should=> %s but was %s\nTEST SUCCEEDS = ****%s****"
    msg.format(as.mkString(", "), expect, res, expect == res)
  }

  def main(args: Array[String]) = {

    println("\nInt test cases")
    val cmp_int = ( (a :Int, b : Int) => a <= b)
    intTestCases().foreach(x => {
      println(evalTestCase(x._1, cmp_int, x._2))
    })

    println("\nString test cases")
    val cmp_str = ( (a :String, b : String) => a <= b)
    strTestCases().foreach(x => {
      println(evalTestCase(x._1, cmp_str, x._2))
    })
  }
}