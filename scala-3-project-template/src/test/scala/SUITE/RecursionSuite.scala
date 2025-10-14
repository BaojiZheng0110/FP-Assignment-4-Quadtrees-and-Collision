package recursion

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import Recursion._
import Recursion.ExprTree._

class RecursionSuite extends ScalaCheckSuite {

  // ----------------------------------
  // Part 1: sumOnes tests
  // ----------------------------------

  test("sumOnes works for small inputs") {
    assertEquals(sumOnes(0), 0)
    assertEquals(sumOnes(5), 5)
    assertEquals(sumOnes(10), 10)
  }

  test("sumOnes throws StackOverflowError for large input") {
    try {
      sumOnes(10000000)
      fail("No exception")
    } catch {
      case _: StackOverflowError => () // expected
      case _: Throwable => fail("Different exception")
    }
  }

  property("sumOnesTail matches sumOnes for many inputs") {
    forAll(Gen.choose(0, 10000)) { n =>
      sumOnes(n) == sumOnesTail(n)
    }
  }

  test("sumOnesTail works for large input without overflow") {
    val result = sumOnesTail(10000000)
    assertEquals(result, 10000000)
  }

  // ----------------------------------
  // Part 2: countOnes tests
  // ----------------------------------

  test("countOnes works for small lists") {
    assertEquals(countOnes(List(1,0,1,1)), 3)
    assertEquals(countOnes(Nil), 0)
  }

  test("countOnes throws StackOverflowError for large list") {
    val bigList = List.fill(10000000)(1)
    try {
      countOnes(bigList)
      fail("No exception")
    } catch {
      case _: StackOverflowError => ()
      case _: Throwable => fail("Different exception")
    }
  }

  property("countOnesTail matches countOnes for many lists") {
    forAll(Gen.listOf(Gen.oneOf(0,1))) { xs =>
      countOnes(xs) == countOnesTail(xs)
    }
  }

  test("countOnesTail works for large list without overflow") {
    val bigList = List.fill(10000000)(1)
    assertEquals(countOnesTail(bigList), 10000000)
  }

  // ----------------------------------
  // Part 3: eval and evalTail tests
  // ----------------------------------

  test("eval works on basic trees") {
    val t1 = Add(Num(1), Num(2))
    val t2 = Mul(Sub(Num(5), Num(3)), Num(4)) // (5 - 3) * 4 = 8
    assertEquals(eval(t1), Some(3.0))
    assertEquals(eval(t2), Some(8.0))
  }

  // Generator for random expression trees
  def genTree: Gen[ExprTree] = Gen.lzy {
    Gen.oneOf(
      Gen.choose(-100, 100).map(Num(_)),
      for {
        l <- genTree
        r <- genTree
        op <- Gen.oneOf(Add(l, r), Sub(l, r), Mul(l, r), Div(l, r))
      } yield op
    )
  }

  implicit lazy val arbTree: Arbitrary[ExprTree] = Arbitrary(genTree)

  property("evalTail matches eval for generated trees") {
    forAll { t: ExprTree =>
      eval(t) == evalTail(t)
    }
  }

  test("evalTail handles large skinny tree without overflow") {
    // Create a very deep skinny tree
    val depth = 100000
    val tree = (1 to depth).foldLeft[ExprTree](Num(1))((acc, _) => Add(acc, Num(1)))
    val res = evalTail(tree)
    assert(res.contains(depth + 1))
  }

}
