package filterdesigner

sealed trait Complex {
  val re: Double
  val im: Double
  val rho: Double
  val theta: Double
  
  def +(c: Complex) = RectComplex(re + c.re, im + c.im)
  def -(c: Complex) = RectComplex(re - c.re, im - c.im)
  def *(c: Complex) = RectComplex(re*c.re - im*c.im, re*c.im + c.re*im) 
  def /(c: Complex) = {
    val den = c.re*c.re + c.im*c.im
    RectComplex((re*c.re + im*c.im)/den, (c.re*im - re*c.im)/den)
  }
  def **(c: Complex) = {if(c.im != 0.0) throw new Exception("Can only take real power of complex number")
  						else new PolarComplex(Math.pow(rho, c.re), theta * c.re)} 
  def par(c: Complex) = (this*c) / (this+c)
  def unary_- = RectComplex(-re, -im)  
  
  override def equals(cpx: Any): Boolean
}

object Complex { 
  implicit def toCpx[T](t: T)(implicit i: Numeric[T]): Complex = RectComplex(i toDouble t, 0)
  val I = RectComplex(0, 1)
  val Zero = RectComplex(0, 0)
  val One = RectComplex(1, 0)
}

class ComplexProxy(val value: Complex) extends Complex {
  val re = value.re; val im = value.im; val rho = value.rho; val theta = value.theta
  override def equals(cpx: Any) = value equals cpx
}

case class RectComplex(val re: Double, val im: Double) extends Complex {
  val rho = Math.sqrt(re * re + im * im)
  val theta = Math.atan2(im, re)
  override def equals(cpx: Any) = cpx match {case cpx: Complex => (re == cpx.re) && (im == cpx.im); case _ => false}  
}

case class PolarComplex(val rho: Double, val theta: Double) extends Complex {
  val re = rho * Math.cos(theta)
  val im = rho * Math.sin(theta)
  override def equals(cpx: Any) = cpx match {case cpx: Complex => (rho == cpx.rho) && (theta == cpx.theta); case _ => false}  
}

sealed trait ComplexExpression {  
  type VarSubst = ComplexExpression.VarSubst
  def +(c: ComplexExpression) = Plus(this, c) simplify
  def -(c: ComplexExpression) = Minus(this, c) simplify
  def *(c: ComplexExpression) = Times(this, c) simplify
  def /(c: ComplexExpression) = Div(this, c) simplify
  def **(c: ComplexExpression) = Pow(this, c) simplify
  def isZero = this match {
    case Constant(c) => c == RectComplex(0, 0)
    case _ => false
  }
  def isOne = this match {
    case Constant(c) => c == RectComplex(1, 0)
    case _ => false
  }
  def unary_- = Neg(this) simplify
  def subst(subst: VarSubst): ComplexExpression  
  val subExprs: Seq[ComplexExpression]
 
  def simplify = subst(Map())
  def nbTerms: Int
  override def toString: String 
  
  def leafs: Seq[ComplexExpression] = subExprs match {
    case Seq() => Seq(this)
    case x => x flatMap {_.leafs}
  }
  def variables = leafs collect {case v: Variable => v} distinct
}

object ComplexExpression {
  type VarSubst = PartialFunction[String, Complex]
  implicit def fromCpx(c: Complex): ComplexExpression = Constant(c)
  implicit def fromString(s: String): ComplexExpression = Variable(s)
  implicit def fromNum[T](t: T)(implicit i: Numeric[T]): ComplexExpression = Constant(RectComplex(i toDouble t, 0)) 
}

trait SingleTerm { this: ComplexExpression => def nbTerms = 1 ; val subExprs = Nil}
trait Binary { this: ComplexExpression =>
  val left: ComplexExpression
  val right: ComplexExpression
  def nbTerms = left.nbTerms + right.nbTerms
  val subExprs = Seq(left, right)
}

case class Constant(value: Complex) extends ComplexExpression with SingleTerm { 
  override def toString = value.toString
  def subst(v: VarSubst) = this
}

case class Variable(label: String) extends ComplexExpression with SingleTerm { 
  override def toString = label
  def subst(v: VarSubst) = if(v isDefinedAt label) v(label) else this
}

case class Pow(val left: ComplexExpression, val right: ComplexExpression) extends ComplexExpression with Binary {
  import ComplexExpression._
  override def toString = "(%s ** %s)" format(left, right)  
  def subst(s: VarSubst) = (left subst s, right subst s) match {
    case (Constant(l), Constant(r)) =>
      if(l == Complex.Zero) 0 else if(r == Complex.Zero) 1 else if(r == Complex.One) l else if (l == Complex.One) 1 else l ** r
    case (l, r) =>
      if(l.isZero) 0 else if(r.isZero) 1 else if(r.isOne) l else if (l.isOne) 1 else Pow(l, r)
  }
}

case class Plus(val left: ComplexExpression, val right: ComplexExpression) extends ComplexExpression with Binary {
  import ComplexExpression._
  def subst(s: VarSubst) = (left subst s, right subst s) match {
    case (Constant(l), Constant(r)) => l + r
    case (l, r) if r.isZero => l
    case (l, r) if l.isZero => r
    case (l, r) => Plus(l, r)
    
  }
  override def toString = "(%s + %s)" format(left, right)
}

case class Minus(val left: ComplexExpression, val right: ComplexExpression) extends ComplexExpression with Binary {
  import ComplexExpression._
  def subst(s: VarSubst) = (left subst s, right subst s) match {
    case (Constant(l), Constant(r)) => l - r
    case (l, r) if r.isZero => l
    case (l, r) if l.isZero => -r
    case (l, r) => Minus(l, r)
    
  }
  override def toString = "(%s - %s)" format(left, right)
}

case class Times(val left: ComplexExpression, val right: ComplexExpression) extends ComplexExpression with Binary {
  import ComplexExpression._
  def subst(s: VarSubst) = (left subst s, right subst s) match {
    case (Constant(l), Constant(r)) => l * r
    case (l, r) if r.isZero => 0
    case (l, r) if l.isZero => 0
    case (l, r) if r.isOne => l
    case (l, r) if l.isOne => r
    case (l, r) => Times(l, r)
  }
  override def toString = "(%s * %s)" format(left, right)
}

case class Div(val left: ComplexExpression, val right: ComplexExpression) extends ComplexExpression with Binary {
  import ComplexExpression._
  def subst(s: VarSubst) = (left subst s, right subst s) match {
    case (Constant(l), Constant(r)) => l / r    
    case (l, r) if l.isZero => 0
    case (l, r) if r.isOne => l    
    case (l, r) => Div(l, r)    
  }
  override def toString = "(%s * %s)" format(left, right)
}


case class Neg(x: ComplexExpression) extends ComplexExpression {
  import ComplexExpression._
  def nbTerms = x.nbTerms
  val subExprs = Seq(x)
  def subst(s: VarSubst) = x subst s match {
    case c: Constant if c.isZero => 0    
    case Neg(x) => x
    case Constant(x) => -x
    case x => Neg(x)
  }
  override def toString = "-" + x
}
