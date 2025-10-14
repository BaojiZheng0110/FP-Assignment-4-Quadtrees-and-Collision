package quadtree

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import QuadTreeFns._   // import functions & helpers

class QuadTreeSuite extends ScalaCheckSuite {

  // Generators for Circle
  val circleGen: Gen[Circle] = for {
    r <- Gen.choose(0.1, 2.0)
    x <- Gen.choose(-10.0 + r, 10.0 - r)
    y <- Gen.choose(-10.0 + r, 10.0 - r)
  } yield Circle(Point(x, y), r)

  implicit lazy val circleArbitrary: Arbitrary[Circle] = Arbitrary(circleGen)

  val circleListGen: Gen[List[Circle]] = for {
    n <- Gen.choose(10, 20)
    circles <- Gen.listOfN(n, circleGen)
  } yield circles

  implicit lazy val circleListArbitrary: Arbitrary[List[Circle]] = Arbitrary(circleListGen)

  // Root square covering [-10, 10]^2
  val world: Square = Square(Point(-10.0, 10.0), 20.0)

  // Geometry sanity check
  test("Circle intersection works") {
    val a = Circle(Point(0, 0), 1.0)
    val b = Circle(Point(1, 0), 1.0)
    assert(circlesIntersect(a, b))
  }

  // Property test
  property("findCollisionsInTree equals findCollisionsInList as sets") {
    forAllNoShrink { (targets: List[Circle], probe: Circle) =>
      val tree = toQuadTree(world, targets)
      val slow = findCollisionsInList(targets, probe).toSet
      val fast = findCollisionsInTree(tree, probe).toSet
      assertEquals(fast, slow)
    }
  }
}
