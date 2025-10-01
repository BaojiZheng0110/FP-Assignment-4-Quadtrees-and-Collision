package quadtree

case class Point(x: Double, y: Double)

case class Circles(center: Point, radius: Double)

case class Squares(topLeft: Point, sideLength: Double)

def CheckCirclesIntersect(c1: Circles, c2: Circles): Boolean = {
    double distSq = (c1.center.x - c2.center.x) **2 + (c1.center.y - c2.center.y) **2

    if (distSq <= (c1.radius - c2.radius) **2) {
        return true
    }
}

def CheckCirclesSquaresIntersect()