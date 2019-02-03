import org.scalatest.{Matchers, FunSuite}

/** @version 1.1.0 */
class ParensTest extends FunSuite with Matchers {

  test("Empty case") {
    Parens.balance("") should be(0)
  }

  test("Left paren") {
    ////pending
    Parens.balance("(") should be(1)
  }

  test("Right paren") {
    //pending
    Parens.balance(")") should be(-1)
  }

  test("Left Right paren") {
    //pending
    Parens.balance("()") should be(0)
  }

  test("Right Left paren") {
    //pending
    Parens.balance(")(") should be(-1)
  }

  test("Right Left Right Right Right paren") {
    //pending
    Parens.balance(")()))") should be(-1)
  }

  test("Left Right Left Left Left paren") {
    //pending
    Parens.balance("()(((") should be(3)
  }

  test("Left Right Left Left Left and opposite") {
    //pending
    Parens.balance("()((()()))") should be(0)
  }

  test("Left Right Left Left Left and opposite then Right Left") {
    //pending
    Parens.balance("()((()())))(") should be(-1)
  }

  test("Either - Empty case") {
    Parens.balanceE("") should be(Right(0))
  }

  test("Either - Illegal") {
    Parens.balanceE("a;sdlfkj") should be(Left("illegal char 'a' at index 0"))
  }

  test("Either - Left paren") {
    //pending
    Parens.balanceE("(") should be(Right(1))
  }

  test("Either - Right paren") {
    //pending
    Parens.balanceE(")") should be(Left("unbalanced at index 0"))
  }

  test("Either - Left Right paren") {
    //pending
    Parens.balanceE("()") should be(Right(0))
  }

  test("Either - Right Left paren") {
    //pending
    Parens.balanceE(")(") should be(Left("unbalanced at index 0"))
  }

  test("Either - Right Left Right Right Right paren") {
    //pending
    Parens.balanceE(")()))") should be(Left("unbalanced at index 0"))
  }

  test("Either - Left Right Left Left Left paren") {
    //pending
    Parens.balanceE("()(((") should be(Right(3))
  }

  test("Either - Left Right Left Left Left and opposite") {
    //pending
    Parens.balanceE("()((()()))") should be(Right(0))
  }

  test("Either - Left Right Left Left Left and opposite then Right Left and ignored illegal") {
    //pending
    Parens.balanceE("()((()())))*(") should be(Left("unbalanced at index 10"))
  }

    test("Either - Left Right Left Left Left and opposite then illegal") {
    //pending
    Parens.balanceE("()((()()))*") should be(Left("illegal char '*' at index 10"))
  }

  test("break up strings - empty") {
    Parens.breakup("", 5) should be ((Seq("", "", "", "", ""), 0))
  }

  test("break up strings - empty no parts") {
    Parens.breakup("", 0) should be ((Seq(""), 0))
  }

  test("break up strings - empty no parts by default") {
    Parens.breakup("", -14) should be ((Seq(""), 0))
  }

  test("break up strings - small") {
    Parens.breakup("mary", 5) should be ((Seq("", "", "", "", "mary"), 0))
  }

  test("break up strings - small 0") {
    Parens.breakup("mary", 0) should be ((Seq("mary"), 4))
  }

  test("break up strings - large 0") {
    Parens.breakup("mary had a little lamb, little lamb little lamb", 0) should
    be ((Seq("mary had a little lamb, little lamb little lamb"), 47))
  }

  test("break up strings - large 5") {
    Parens.breakup("mary had a little lamb, little lamb little lamb", 5) should
           be (Seq("mary had ", "a little ", "lamb, lit", "tle lamb ", "little lamb"), 9)
  }

  test("Recursive - Empty case") {
    //pending
    Parens.balanceR("", 5) should be(Right(0))
  }

  test("Recursive - Illegal") {
    //pending
    Parens.balanceR("a;sdlfkj", 5) should be(Left("illegal char at index 0"))
  }

  test("Recursive - Left paren") {
    //pending
    Parens.balanceR("(", 5) should be(Right(1))
  }

  test("Recursive - Right paren") {
    //pending
    Parens.balanceR(")", 5) should be(Left("unbalanced after index 0"))
  }

  test("Recursive - Left Right paren") {
    //pending
    Parens.balanceR("()", 5) should be(Right(0))
  }

  test("Recursive - Right Left paren") {
    //pending
    Parens.balanceR(")(", 5) should be(Left("unbalanced after index 0"))
  }

  test("Recursive - Right Left Right Right Right paren") {
    //pending
    Parens.balanceR(")()))", 5) should be(Left("unbalanced after index 0"))
  }

  test("Recursive - Left Right Left Left Left paren") {
    //pending
    Parens.balanceR("()(((", 5) should be(Right(3))
  }

  test("Recursive - Left Right Left Left Left and opposite") {
    //pending
    Parens.balanceR("()((()()))", 5) should be(Right(0))
  }

  test("Recursive - Left Right Left Left Left and opposite then Right Left and ignored illegal") {
    //pending
    Parens.balanceR("()((()())))*(", 5) should be(Left("illegal char at index 11"))
  }

  test("Recursive - Left Right Left Left Left and opposite then illegal") {
    //pending
    Parens.balanceR("()((()()))*", 5) should be(Left("illegal char at index 10"))
  }


}
