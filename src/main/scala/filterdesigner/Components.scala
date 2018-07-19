package filterdesigner

import Complex._

trait Node

case class Net(number: Int) extends Node
case object Net {
  val Ground = Net(-1)
}

trait Component extends Node 

trait Passive extends Component {
  def admittance: ComplexExpression  
  protected val omega: ComplexExpression = Constant(Math.PI * 2) * "f" 
}

class Source extends Component
class OpAmp extends Component

class Resistor(value: ComplexExpression) extends Passive {
  def admittance = Constant(1) / value
}

class Capacitor(value: ComplexExpression) extends Passive {
  def admittance = value * omega * I
}

class Inductor(value: ComplexExpression) extends Passive {
  def admittance = Constant(-I) / (omega * value)
}

trait Circuit {
  val nets: Map[Component, IndexedSeq[Net]]
  val nbNets: Int
}
