package quadtree

// ===== Data types =====
case class Point(x: Double, y: Double)
case class Circle(center: Point, radius: Double)

// define Square by its top-left corner and side length
//   left = topLeft.x
//   right = topLeft.x + sideLength
//   top = topLeft.y
//   bottom = topLeft.y - sideLength
case class Square(topLeft: Point, sideLength: Double)

// A QuadTree node: boundary square, items stored at this node.
// and either 0 or 4 children (order: NW, NE, SW, SE).
case class QuadTree(boundary: Square, items: List[Circle], subs: List[QuadTree])

object QuadTreeFns {

  val Capacity: Int = 5

  private def sq(x: Double): Double = x * x

  // squared distance between two points (avoids sqrt)
  private def dist2(a: Point, b: Point): Double =
    sq(a.x - b.x) + sq(a.y - b.y)

  private def clamp(v: Double, lo: Double, hi: Double): Double =
    if (v < lo) lo else if (v > hi) hi else v

  def circlesIntersect(c1: Circle, c2: Circle): Boolean = {
    val rsum = c1.radius + c2.radius
    dist2(c1.center, c2.center) <= sq(rsum)
  }

  private def closestPointInSquare(p: Point, s: Square): Point = {
    val left   = s.topLeft.x
    val right  = s.topLeft.x + s.sideLength
    val top    = s.topLeft.y
    val bottom = s.topLeft.y - s.sideLength
    Point(
      clamp(p.x, left, right),
      clamp(p.y, bottom, top)
    )
  }

  def circleIntersectsSquare(c: Circle, s: Square): Boolean = {
    val q = closestPointInSquare(c.center, s)
    dist2(c.center, q) <= sq(c.radius)
  }

  def circleInsideSquare(c: Circle, s: Square): Boolean = {
    val left   = s.topLeft.x
    val right  = s.topLeft.x + s.sideLength
    val top    = s.topLeft.y
    val bottom = s.topLeft.y - s.sideLength
    val x = c.center.x
    val y = c.center.y
    (x - c.radius >= left) &&
    (x + c.radius <= right) &&
    (y + c.radius <= top) &&
    (y - c.radius >= bottom)
  }

  //Subdivision (NW, NE, SW, SE)
  def subdivide(s: Square): List[Square] = {
    val half = s.sideLength / 2.0
    val left   = s.topLeft.x
    val top    = s.topLeft.y
    val nw = Square(Point(left,           top),            half)       
    val ne = Square(Point(left + half,    top),            half)       
    val sw = Square(Point(left,           top - half),     half)       
    val se = Square(Point(left + half,    top - half),     half)       
    nw :: ne :: sw :: se :: Nil
  }

  //List helpers
  private def hasAtLeast[A](xs: List[A], n: Int): Boolean = {
    def loop(ys: List[A], k: Int): Boolean = ys match {
      case _ :: tail => if (k <= 1) true else loop(tail, k - 1)
      case Nil => false
    }
    if (n <= 0) true else loop(xs, n)
  }

  // prepend all ys onto xs (ys reversed) — used to avoid ++ in some places if desired
  private def append(a: List[Circle], b: List[Circle]): List[Circle] =
    if (a == Nil) b
    else a.head :: append(a.tail, b)

  // filter using recursion
  private def filterIntersecting(xs: List[Circle], probe: Circle, acc: List[Circle] = Nil): List[Circle] = xs match {
    case Nil => reverse(acc)
    case h :: t =>
      if (circlesIntersect(h, probe)) filterIntersecting(t, probe, h :: acc)
      else filterIntersecting(t, probe, acc)
  }

  // simple reverse (tail-recursive)
  private def reverse[A](xs: List[A]): List[A] = {
    def go(ys: List[A], acc: List[A]): List[A] = ys match {
      case Nil => acc
      case h :: t => go(t, h :: acc)
    }
    go(xs, Nil)
  }

  // Building & inserting
  def empty(boundary: Square): QuadTree =
    QuadTree(boundary, Nil, Nil)

  // Decide which single child fully contains the circle; return its index 0..3 (NW,NE,SW,SE)
  private def childIndexIfAny(children: List[Square], c: Circle, idx: Int = 0): Option[Int] =
    children match {
      case Nil => None
      case h :: t =>
        if (circleInsideSquare(c, h)) Some(idx)
        else childIndexIfAny(t, c, idx + 1)
    }


  private def distributeToChildren(xs: List[Circle],
    childSquares: List[Square],
    // accumulating child items in the same order NW,NE,SW,SE:
    c0: List[Circle], c1: List[Circle], c2: List[Circle], c3: List[Circle],
    stay: List[Circle]): (List[Circle], List[Circle], List[Circle], List[Circle], List[Circle]) = xs match {
    case Nil =>
      (reverse(c0), reverse(c1), reverse(c2), reverse(c3), reverse(stay))
    case h :: t =>
      childIndexIfAny(childSquares, h) match {
        case Some(0) => distributeToChildren(t, childSquares, h :: c0, c1, c2, c3, stay)
        case Some(1) => distributeToChildren(t, childSquares, c0, h :: c1, c2, c3, stay)
        case Some(2) => distributeToChildren(t, childSquares, c0, c1, h :: c2, c3, stay)
        case Some(3) => distributeToChildren(t, childSquares, c0, c1, c2, h :: c3, stay)
        case _       => distributeToChildren(t, childSquares, c0, c1, c2, c3, h :: stay)
      }
  }

  // Insert a single circle into the quadtree (functional: returns a new tree).
  def insert(qt: QuadTree, target: Circle): QuadTree = {
    // 1) If target not fully inside this node's boundary, do nothing.
    if (!circleInsideSquare(target, qt.boundary)) qt
    else qt.subs match {
      // 2) Leaf node: add to items; if over capacity, subdivide and push down.
      case Nil =>
        val withNew = target :: qt.items
        if (hasAtLeast(withNew, Capacity + 1)) {
          val childSquares = subdivide(qt.boundary) // NW, NE, SW, SE
          val emptyChildren =
            QuadTree(childSquares.head, Nil, Nil) ::
            QuadTree(childSquares.tail.head, Nil, Nil) ::
            QuadTree(childSquares.tail.tail.head, Nil, Nil) ::
            QuadTree(childSquares.tail.tail.tail.head, Nil, Nil) :: Nil

          // Distribute items (including the newly added one)
          val buckets = distributeToChildren(withNew, childSquares, Nil, Nil, Nil, Nil, Nil)
          val newSubs = {
            val n0 = fillAll(emptyChildren.head, buckets._1)
            val n1 = fillAll(emptyChildren.tail.head, buckets._2)
            val n2 = fillAll(emptyChildren.tail.tail.head, buckets._3)
            val n3 = fillAll(emptyChildren.tail.tail.tail.head, buckets._4)
            n0 :: n1 :: n2 :: n3 :: Nil
          }
          QuadTree(qt.boundary, buckets._5, newSubs)
        } else {
          QuadTree(qt.boundary, withNew, Nil)
        }

      // 3) Internal node: try to descend if exactly one child fully contains it; else keep at this node.
      case a :: b :: c :: d :: Nil =>
        val childSquares =
          a.boundary :: b.boundary :: c.boundary :: d.boundary :: Nil

        childIndexIfAny(childSquares, target) match {
          case Some(0) => QuadTree(qt.boundary, qt.items, insert(a, target) :: b :: c :: d :: Nil)
          case Some(1) => QuadTree(qt.boundary, qt.items, a :: insert(b, target) :: c :: d :: Nil)
          case Some(2) => QuadTree(qt.boundary, qt.items, a :: b :: insert(c, target) :: d :: Nil)
          case Some(3) => QuadTree(qt.boundary, qt.items, a :: b :: c :: insert(d, target) :: Nil)
          case _       => QuadTree(qt.boundary, target :: qt.items, qt.subs)
        }

      // 4) Invariant enforcement: a node must have 0 or 4 children.
      case _ =>
        // If something went wrong elsewhere, keep the node unchanged (or throw).
        qt
    }
  }

  // Helper: insert a list of items into a (fresh) subtree
  private def fillAll(q: QuadTree, items: List[Circle]): QuadTree = items match {
    case Nil     => q
    case h :: t  => fillAll(insert(q, h), t)
  }

  // Build a quadtree from a list of targets and a root boundary.
  def toQuadTree(rootBoundary: Square, targets: List[Circle]): QuadTree =
    fillAll(empty(rootBoundary), targets)

  // Querying
  // Slow baseline: scan a list to find all intersecting targets.
  def findCollisionsInList(targets: List[Circle], probe: Circle): List[Circle] =
    filterIntersecting(targets, probe)

  // Fast version using the tree + pruning.
  def findCollisionsInTree(qt: QuadTree, probe: Circle): List[Circle] = {
    if (!circleIntersectsSquare(probe, qt.boundary)) Nil
    else {
      val here = filterIntersecting(qt.items, probe)
      qt.subs match {
        case Nil => here
        case a :: b :: c :: d :: Nil =>
          val ab = append(findCollisionsInTree(a, probe), findCollisionsInTree(b, probe))
          val cd = append(findCollisionsInTree(c, probe), findCollisionsInTree(d, probe))
          append(here, append(ab, cd))
        case _ =>
          here
      }
    }
  }
}
