package coursera.progfun.patmatch

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  override def toString = Expr.show(this)
}

object Expr {
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Prod(e1: Sum, e2) => s"(${show(e1)}) * ${show(e2)}"
    case Prod(e1, e2: Sum) => s"${show(e1)} * (${show(e2)})"
    case Prod(e1, e2) => s"${show(e1)} * ${show(e2)}"
    case Var(name) => name
  }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(name: String) extends Expr
