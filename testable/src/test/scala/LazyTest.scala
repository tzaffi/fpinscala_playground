import org.scalatest.{Matchers, FunSuite}

/** @version 1.2.0 */
class LazyTest extends FunSuite with Matchers {
  def zc[A](hd: =>A, tl: => ZStream[A]): ZStream[A] = ZStream.cons[A](hd,tl)
  def ze[A]: ZStream[A] = ZStream.empty[A]

  def sleepyInt(x: Int): Int = {
    Thread.sleep(100)
    println("lazy = " + x)
    x
  }

  def mkSleepyInt(x: Int): () => Int = () => sleepyInt(x)

  def buildExample(n: Int): ZStream[Int] = {
    @annotation.tailrec
    def loop(n: Int, res: ZStream[Int]): ZStream[Int] = 
      if (n <= 0) res
      else loop(n-1, zc(sleepyInt(n), res))
    loop(n, ze)
  }

  // test("Empty toList is an empty list") {
  //   (ZEmpty: ZStream[Int]).toList should be (List[Int]())
  // }

  // test("toList of simple values") {
  //   ZStream(1,2,3).toList should be (List(1,2,3))
  // }

  // test("toList of lazy values using cons") {
  //   val zs = zc(sleepyInt(1), zc(sleepyInt(2), zc(sleepyInt(3), ze)))
  //   println("did this guy get printed before the lazy's did?")
  //   zs.toList should be (List(1,2,3))
  // }

  // test("toList of lazy values using lazyApply") {
  //   val zs = ZStream.lazyApply(mkSleepyInt(1), mkSleepyInt(2), mkSleepyInt(3))
  //   println("did this guy get printed before the lazy's did?")
  //   zs.toList should be (List(1,2,3))
  // }

  // test("buildExample") {
  //   buildExample(5).toList should be (ZStream.lazyApply(mkSleepyInt(1), mkSleepyInt(2), mkSleepyInt(3), mkSleepyInt(4), mkSleepyInt(5)).toList)
  // }

  // test("take keeps the first n elements of a Stream") {
  //   println("did this guy get printed before the lazy's did?")
  //   buildExample(5).take(2).toList should be (buildExample(2).toList)
  // }

  // test("drop skips the first n elements of a Stream") {
  //   println("did this guy get printed before the lazy's did?")
  //   buildExample(5).drop(2).toList should be (ZStream.lazyApply(mkSleepyInt(3), mkSleepyInt(4), mkSleepyInt(5)).toList)
  // }

  // test("takeWhile keeps until it encounters a number that doesn't divide 12"){
  //   def divBy12 = ( (x:Int) => (12 % x == 0))
  //   println("did this guy get printed before the lazy's did?")
  //   val to12 = buildExample(12)
  //   println("and what about this?")
  //   to12.takeWhile(divBy12).toList should be (buildExample(4).toList)
  // }

  // test("forAll divisible by 12"){
  //   def divBy12 = ( (x:Int) => (12 % x == 0))

  //   buildExample(4).forAll(divBy12) should be (true)
  //   buildExample(7).forAll(divBy12) should be (false)
  // }

  // test("takeWhileUsingFoldRight similar to the above"){
  //   def divBy12 = ( (x:Int) => (12 % x == 0))
  //   println("did this guy get printed before the lazy's did?")
  //   val to12 = buildExample(12)
  //   println("and what about this?")
  //   to12.takeWhileUsingFoldRight(divBy12).toList should be (buildExample(4).toList)
  // }

  // test("headOption of an empty list is None"){
  //   ze[Int].headOption should be (None: Option[Int])
  // }

  // test("headOption of a non-empty list is Some of the first"){
  //   val to3 = buildExample(3)
  //   println("should not evaluate 2 or 3:")
  //   to3.headOption should be (Some(1))
  // }

  test("map should take the squares"){
    buildExample(3).map(x=>x*x).toList should be (ZStream.lazyApply(mkSleepyInt(1), mkSleepyInt(4), mkSleepyInt(9)).toList)
  }

  test("filter odds out should keep the evens"){
    buildExample(3).filter(_%2==0).toList should be (ZStream.lazyApply(mkSleepyInt(2)).toList)
  }

  test("append does what you think"){
    (buildExample(3).append(buildExample(2))).toList should be (
      ZStream.lazyApply(mkSleepyInt(1), mkSleepyInt(2), mkSleepyInt(3), mkSleepyInt(1), mkSleepyInt(2)).toList
    )
  }

  test("flatMap should expand in a wierd way"){
    (buildExample(3) flatMap(buildExample(_))).toList should be (
      (buildExample(1) append buildExample(2) append buildExample(3)).toList
    )
  }
}
