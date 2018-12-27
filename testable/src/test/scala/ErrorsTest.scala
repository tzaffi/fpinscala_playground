import org.scalatest.{Matchers, FunSuite}

/** @version 1.2.0 */
class ErrorsTest extends FunSuite with Matchers {

  /********* OPTION *********/

  test("map of None is None") {
    val nada = ZNone
    def f(x: Integer): Integer =
      x + 1

    nada.map(f) should be (nada)
  }

  test("map of Something works") {
    def f(x: Int): Int =
      x + 1

    ZSome(3).map(f) should be (ZSome(4))
  }

  test("getOrElse of nothing is the default") {
    ZNone.getOrElse(42) should be (42)
  }

  test("getOrElse of something is the thing") {
    ZSome(13).getOrElse(42) should be (13)
  }

  test("flatMap of None is None") {
    val nada = ZNone
    def f(x: Int): ZOption[Int] =
      ZSome(x + 1)

    nada.flatMap(f) should be (nada)
  }

  test("flatMap of Something works") {
    val nada = ZNone
    def f(x: Int): ZOption[Int] =
      ZSome(x + 1)

    ZSome(3).flatMap(f) should be (ZSome(4))
  }

  test("flatMap returns None when f undefined") {
    val nada = ZNone
    def f(x: Int): ZOption[Integer] =
      if (x % 2 == 0)
        ZSome(x + 1)
      else
        ZNone

    ZSome(2).flatMap(f) should be (ZSome(3))
    ZSome(3).flatMap(f) should be (ZNone)
  }

  test("orElse of nothing is the default") {
    ZNone orElse(ZSome(42)) should be (ZSome(42))
  }

  test("orElse of something is the Some(thing)") {
    ZSome(13) orElse(ZSome(42)) should be (ZSome(13))
  }

  test("filter keeps None as None") {
    def f(x: Int): Boolean =
      x % 2 == 0

    ZNone filter(f) should be (ZNone)
  }

  test("filter keeps Some(true)") {
    def f(x: Int): Boolean =
      x % 2 == 0

    ZSome(2) filter(f) should be (ZSome(2))
  }

  test("filter discareds Some(false)") {
    def f(x: Int): Boolean =
      x % 2 == 0

    ZSome(3) filter(f) should be (ZNone)
  }

  test("mean of 1 2 3"){
    ZOption.mean(List(1.0, 2.0, 3.0)) should be (ZSome(2.0))
  }

  test("mean of nothing"){
    ZOption.mean(List[Double]()) should be (ZNone)
  }

  test("variance of 1 2 3"){
    ZOption.variance(List(1.0, 2.0, 3.0)) should be (ZSome(2.0/3.0))
  }

  test("variance of nothing"){
    ZOption.variance(List[Double]()) should be (ZNone)
  }

  test("map2 of None None is None"){
    ZOption.map2(ZNone, ZNone)((x: Int, y: Int) => x*y) should be (ZNone)
  }

  test("map2 of None Some is None"){
    ZOption.map2(ZNone, ZSome(3))((x: Int, y: Int) => x*y) should be (ZNone)
  }

  test("map2 of Some None is None"){
    ZOption.map2(ZSome(3), ZNone)((x: Int, y: Int) => x*y) should be (ZNone)
  }

  test("map2 of Some Some is Some"){
    ZOption.map2(ZSome(3), ZSome(4))((x: Int, y: Int) => x*y) should be (ZSome(12))
  }

  test("sequence with 2 None's is None") {
    ZOption.sequence(List(ZSome(13), ZNone, ZSome(42), ZNone)) should be (ZNone)
  }

  test("sequence with even a single None is None") {
    ZOption.sequence(List(ZSome(13), ZSome(-11), ZSome(42), ZNone)) should be (ZNone)
  }

  test("sequence with all ZSome's should be ZSome") {
    ZOption.sequence(List(ZSome(13), ZSome(-11), ZSome(42), ZSome(14))) should be (ZSome(List(13, -11, 42, 14)))
  }

  test("Nil sequence should be ZSome(Nil)") {
    ZOption.sequence(Nil) should be (ZSome(Nil))
  }

  test("Try when succeeds is Some") {
    ZOption.Try("13".toInt) should be (ZSome(13))
  }

  test("Try when fails is None") {
    ZOption.Try("thirteen".toInt) should be (ZNone)
  }

  test("traverseWithTry with 2 None's is None") {
    ZOption.traverseWithTry(List("13", "no", "42", "no"))(_.toInt) should be (ZNone)
  }

  test("traverseWithTry with a single None's is None") {
    ZOption.traverseWithTry(List("13", "no", "42", "-93"))(_.toInt) should be (ZNone)
  }

  test("traverseWithTry with all goods is Some") {
    ZOption.traverseWithTry(List("13", "1024", "42", "-93"))(_.toInt) should be (ZSome(List(13, 1024, 42, -93)))
  }

  test("Nil traverseWithTry should be ZSome(Nil)") {
    ZOption.traverseWithTry(List[String]())(_.toInt) should be (ZSome(Nil))
  }

  test("traverse with 2 None's is None") {
    val stoi = (x: String) => ZOption.Try(x.toInt)
    ZOption.traverse(List("13", "no", "42", "no"))(stoi) should be (ZNone)
  }

  test("traverse with a single None's is None") {
    val stoi = (x: String) => ZOption.Try(x.toInt)
    ZOption.traverse(List("13", "no", "42", "-93"))(stoi) should be (ZNone)
  }

  test("traverse with all goods is Some") {
    val stoi = (x: String) => ZOption.Try(x.toInt)
    ZOption.traverse(List("13", "1024", "42", "-93"))(stoi) should be (ZSome(List(13, 1024, 42, -93)))
  }

  test("Nil traverse should be ZSome(Nil)") {
    val stoi = (x: String) => ZOption.Try(x.toInt)
    ZOption.traverse(List[String]())(stoi) should be (ZSome(Nil))
  }

  test("sequenceUsingTraverse with 2 None's is None") {
    ZOption.sequenceUsingTraverse(List(ZSome(13), ZNone, ZSome(42), ZNone)) should be (ZNone)
  }

  test("sequenceUsingTraverse with even a single None is None") {
    ZOption.sequenceUsingTraverse(List(ZSome(13), ZSome(-11), ZSome(42), ZNone)) should be (ZNone)
  }

  test("sequenceUsingTraverse with all ZSome's should be ZSome") {
    ZOption.sequenceUsingTraverse(List(ZSome(13), ZSome(-11), ZSome(42), ZSome(14))) should be (ZSome(List(13, -11, 42, 14)))
  }

  test("Nil sequenceUsingTraverse should be ZSome(Nil)") {
    ZOption.sequenceUsingTraverse(Nil) should be (ZSome(Nil))
  }

  test("lift of toString does what you think it should") {
    val ltsI = ZOption.lift((a: Int) => a.toString)
    val ltsS = ZOption.lift((a: String) => a.toString)
    ltsI(ZSome(195)) should be (ZSome("195"))
    ltsS(ZSome("one hundred ninety five")) should be (ZSome("one hundred ninety five"))
  }

  test("lift is useful mathematically speaking") {
    val addL = ZOption.lift[(Double,Double),Double]({case (x,y) => x+y})
    def logO(x: Double): ZOption[Double] = ZOption.Try(Math.log(x))
    logO(3*4) should be (addL(ZSome(Math.log(3), Math.log(4))))
  }

  /********* EITHER  *********/
  test("map of Left is Left") {
    val lefty = ZLeft("you're off the track man!")
    def f(x: Int): Int =
      x + 1

    lefty.map(f) should be (lefty)
  }

  test("map of Something works") {
    def f(x: Int): Int =
      x + 1

    ZRight(3).map(f) should be (ZRight(4))
  }

  test("flatMap") {
    def f(x: Int): ZEither[String, Int] =
      if (x % 2 == 0) ZRight(x + 1)
      else ZLeft("odds are odd")

    val lefty = ZLeft("you're off the track man!")
    lefty.flatMap[String,Int](f) should be (lefty)

    ZRight(2).flatMap[String,Int](f) should be (ZRight(3))
    ZRight(3).flatMap[String,Int](f) should be (ZLeft("odds are odd"))
  }

  test("orElse of Left is default") {
    ZLeft("you're off the track man!").orElse(ZRight(42)) should be (ZRight(42))
    ZLeft("you're off the track man!").orElse(ZLeft("woah")) should be (ZLeft("woah"))
  }

  test("orElse of Right is Right") {
    ZRight(13).orElse(ZRight(42)) should be (ZRight(13))
  }

  test("map2 of Left Left is first Left") {
    val f = (x: Int, y: Int) => x*y
    ZLeft("bad!").map2(ZLeft("42"))(f) should be (ZLeft("bad!"))
  }

  test("map2 of Left Right is first Left") {
    val f = (x: Int, y: Int) => x*y
    ZLeft("bad!").map2(ZRight(42))(f) should be (ZLeft("bad!"))
  }

  test("map2 of Right Left is Left") {
    val f = (x: Int, y: Int) => x*y
    ZRight(13).map2(ZLeft("42"))(f) should be (ZLeft("42"))
  }

  test("map2 of Right Right is Right * Right") {
    val f = (x: Int, y: Int) => x*y
    ZRight(13).map2(ZLeft("42"))(f) should be (ZLeft("42"))
  }

}
