package filterdesigner

import Net._

class NodeAnalysis(val circuit: Circuit, val params: ComplexExpression.VarSubst) {  
  val nbNets = circuit.nbNets
  val comps = circuit.nets.keySet.toArray
  val passive: Array[Passive] = comps collect {case p: Passive => p}
  val sources: Array[Source] = comps collect {case p: Source => p}
  val opamps: Array[OpAmp] = comps collect {case o: OpAmp => o}
  
  val G = new SquareDenseMatrix(nbNets) 
  
  def eval(exp: ComplexExpression) = exp subst params match {case Constant(cpx) => cpx; case _ => throw new Exception("Trying to evaluate non-const expression")}
  for(p <- passive) {
    circuit nets p match {
      case Seq(Ground, Ground) => ()
      case Seq(Ground, Net(i)) => G(i, i) += eval(p.admittance)
      case Seq(Net(i), Ground) => G(i, i) += eval(p.admittance)
      case Seq(Net(i), Net(j)) => G(i, i) += eval(p.admittance)
      							  G(j, j) += eval(p.admittance)
      							  G(i, j) -= eval(p.admittance)
      							  G(j, i) -= eval(p.admittance)
    }    
  }
  
  val B = new DenseMatrix(nbNets, sources.size + opamps.size)
  val C = new DenseMatrix(sources.size + opamps.size, nbNets)
  for(s <- 0 until sources.size) {
    circuit nets sources(s) match {
      case Seq(Net(pos), Net(neg)) => if(pos >= 0) {B(pos, s) := 1; C(s, pos) := 1}
    		  						  if(neg >= 0) {B(neg, s) := -1; C(s, neg) := -1}
    }    		  						  
  }
  
  for(o <- 0 until opamps.size) {
    circuit nets opamps(o) match {
      case Seq(Net(pos), Net(neg), Net(out)) => if(out >= 0) B(out, sources.size + o) := 1
    		  									if(pos >= 0) C(sources.size + o, pos) := 1
    		  								  if(neg >= 0) C(sources.size + o, neg) := -1
    }
  }    
  
  val A = new SquareDenseMatrix(nbNets + sources.size + opamps.size)  
  for(i <- 0 until G.size; j <- 0 until G.size) {    
      A(i, j) := G(i, j)       
  }
  for(i <- 0 until B.rows; j <- 0 until B.cols) {    
      A(i, j + G.size) := B(i, j) 
  }
  for(i <- 0 until C.rows; j <- 0 until C.cols) {    
      A(i + G.size, j) := C(i, j) 
  } 
  
  val z = new Array[Complex](A.size)
  for(i <- 0 until z.size) {    
    if(i >= nbNets && i < nbNets + sources.size) z update(i, 1)
    else z update(i, 0)
  }    
  
  val result = GaussianElimination(A, z)
  
  val getVoltage: Net => Complex = {
    case Ground => 0
    case Net(i) => result(i)
  }
}
