sealed trait ZTree[+A]
case class Leaf[A](value: A) extends ZTree[A]
case class Branch[A](left: ZTree[A], right: ZTree[A]) extends ZTree[A]

object ZTree {

  //3.25 size - counts the number of nodes (leaves + internal)
  def size[A](t: ZTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(x,y) => 1 + size(x) + size(y)
  }

  //3.26 maximum - maximum element in a tree
  def maximum(t: ZTree[Int]): Int = t match {
    case Leaf(z) => z
    case Branch(x,y) => maximum(x) max maximum(y)
  }

  //3.27 depth
  def depth[A](t: ZTree[A]): Int = t match {
    case Leaf(z) => 0
    case Branch(x,y) => 1 + (depth(x) max depth(y))
  }

  //3.28 map
  def map[A,B](t: ZTree[A])(f: A=>B): ZTree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(x,y) => Branch(map(x)(f),map(y)(f))
  }

  //3.29 fold + reimplement the above
  def fold[A,B](t: ZTree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(x,y) => g(fold(x)(f)(g),fold(y)(f)(g))
  }

  def sizeUsingFold[A](t: ZTree[A]): Int = fold(t)(x=>1)(1 + _ + _)
  def maxUsingFold(t: ZTree[Int]): Int = fold(t)(x=>x)(_ max _)
  def depthUsingFold[A](t: ZTree[A]): Int = fold(t)(_=>0)(1+_ max _)
  def mapUsingFold[A,B](t: ZTree[A])(f: A=>B): ZTree[B] = fold(t)(x=>Leaf(f(x)):ZTree[B])(Branch(_,_))


  def root(): ZTree[Int] = Leaf(0)

  def threeLevelBalanced(): ZTree[Int] = {
    Branch(Branch(Branch(Leaf(0),Leaf(1)),Branch(Leaf(2),Leaf(3))),
      Branch(Branch(Leaf(4),Leaf(5)),Branch(Leaf(6),Leaf(7))))
  }

  def threeLevelBalancedStrings(): ZTree[String] = {
    Branch(Branch(Branch(Leaf("0"),Leaf("1")),Branch(Leaf("2"),Leaf("3"))),
      Branch(Branch(Leaf("4"),Leaf("5")),Branch(Leaf("6"),Leaf("7"))))
  }

  def twoLevelBalanced(): ZTree[Int] =
   threeLevelBalanced match { case Branch(_,twoLevel) => twoLevel }

  def threeLevelSkewed(): ZTree[Int] = {
    Branch(Branch(Branch(Leaf(0),Leaf(1)),Leaf(3)),Leaf(4))
  }

  def assertResult[A](a: A)(b: A) = {
    if (a != b) {
      println("should have gotten %s but got %s instead".format(a,b))
      println("[%s] SHOULD BE [%s]".format(a.toString diff b.toString, b.toString diff a.toString))
    }
  }

  def testFold() {
    println("testFold():")
    assertResult(28) { fold(threeLevelBalanced)(x=>x)(_+_) }

    println("mapUsingFold():")
    assertResult(map(threeLevelBalanced)(_.toString))(mapUsingFold(threeLevelBalanced)(_.toString))

    println("depthUsingFold():")
    assertResult(depth(threeLevelBalanced))(depthUsingFold(threeLevelBalanced))
    assertResult(depth(threeLevelBalanced))(depthUsingFold(threeLevelBalanced))
    assertResult(depth(root))(depthUsingFold(root))
    assertResult(depth(threeLevelSkewed))(depthUsingFold(threeLevelSkewed))

    println("maxUsingFold():")
    assertResult(7) { maxUsingFold(threeLevelBalanced) }
    assertResult(7) { maxUsingFold(twoLevelBalanced) }
    assertResult(0) { maxUsingFold(root) }
    assertResult(4) { maxUsingFold(threeLevelSkewed) }

    println("sizeUsingFold():")
    assertResult(15) { sizeUsingFold(threeLevelBalanced) }
    assertResult(7) { sizeUsingFold(twoLevelBalanced) }
    assertResult(1) { sizeUsingFold(root) }
    assertResult(7) { sizeUsingFold(threeLevelSkewed) }
}

  def testMap() {
    println("testMap():")
    assertResult(threeLevelBalancedStrings)(map(threeLevelBalanced)(_.toString))
  }

  def testDepth() {
    println("testDepth():")
    assertResult(3) { depth(threeLevelBalanced) }
    assertResult(2) { depth(twoLevelBalanced) }
    assertResult(0) { depth(root) }
    assertResult(3) { depth(threeLevelSkewed) }
  }

  def testMaximum() {
    println("testMaximum():")
    assertResult(7) { maximum(threeLevelBalanced) }
    assertResult(7) { maximum(twoLevelBalanced) }
    assertResult(0) { maximum(root) }
    assertResult(4) { maximum(threeLevelSkewed) }
  }

  def testSize() = {
    println("testSize():")
    assertResult(15) { size(threeLevelBalanced) }
    assertResult(7) { size(twoLevelBalanced) }
    assertResult(1) { size(root) }
    assertResult(7) { size(threeLevelSkewed) }
  }

  def main() = {
    testSize()
    testMaximum()
    testDepth()
    testMap()
    testFold()
  }

}


