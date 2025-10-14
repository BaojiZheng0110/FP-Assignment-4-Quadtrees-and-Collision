package recursion

import scala.annotation.tailrec



object Recursion {

  // Non-tail recursive: adds up N ones recursively
  def sumOnes(n: Int): Int =
    if (n <= 0) 0
    else 1 + sumOnes(n - 1)

  // Tail recursive version using accumulator
  @tailrec
  def sumOnesTail(n: Int, acc: Int = 0): Int =
    if (n <= 0) acc
    else sumOnesTail(n - 1, acc + 1)



  def countOnes(xs: List[Int]): Int = xs match {
    case Nil => 0
    case 1 :: tail => 1 + countOnes(tail)
    case _ :: tail => countOnes(tail)
  }

  @tailrec
  def countOnesTail(xs: List[Int], acc: Int = 0): Int = xs match {
    case Nil => acc
    case 1 :: tail => countOnesTail(tail, acc + 1)
    case _ :: tail => countOnesTail(tail, acc)
  }



  sealed trait ExprTree
  object ExprTree {
    case class Num(value: Int) extends ExprTree
    case class Add(l: ExprTree, r: ExprTree) extends ExprTree
    case class Sub(l: ExprTree, r: ExprTree) extends ExprTree
    case class Mul(l: ExprTree, r: ExprTree) extends ExprTree
    case class Div(l: ExprTree, r: ExprTree) extends ExprTree
  }

  import ExprTree._

  // Non-tail recursive eval
  def eval(e: ExprTree): Option[Double] = e match {
    case Num(v) => Some(v.toDouble)
    case Add(l, r) => for (lv <- eval(l); rv <- eval(r)) yield lv + rv
    case Sub(l, r) => for (lv <- eval(l); rv <- eval(r)) yield lv - rv
    case Mul(l, r) => for (lv <- eval(l); rv <- eval(r)) yield lv * rv
    case Div(l, r) => for {
      lv <- eval(l)
      rv <- eval(r)
      if rv != 0
    } yield lv / rv
  }

  // Tail-recursive CPS eval
  def evalTail(e: ExprTree): Option[Double] = {
    def loop(expr: ExprTree, k: Double => Option[Double]): Option[Double] = expr match {
      case Num(v) => k(v.toDouble)
      case Add(l, r) => loop(l, lv => loop(r, rv => k(lv + rv)))
      case Sub(l, r) => loop(l, lv => loop(r, rv => k(lv - rv)))
      case Mul(l, r) => loop(l, lv => loop(r, rv => k(lv * rv)))
      case Div(l, r) => loop(l, lv => loop(r, rv =>
        if (rv != 0) k(lv / rv) else None
      ))
    }
    loop(e, x => Some(x))
  }

}
