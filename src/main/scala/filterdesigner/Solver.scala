package filterdesigner


object GaussianElimination {
  def apply(A: SquareDenseMatrix, b: Array[Complex]) = {
    val M = new DenseMatrix(A.size, A.size + 1)
    for(i <- 0 until A.size; j <- 0 to A.size)       
    	M(i, j) := (if(j < A.size) A(i, j) else b(i)) 
    
    val zero = M.zero
    for(k <- 0 until (A.size - 1); i <- (k+1) until A.size) if(M(i, k) != zero) {
      M(i, k) /= M(k, k)
      for(j <- (k+1) to A.size) {
        M(i, j) -= M(i, k) * M(k, j)
      }
    }
    
    val x = new Array[Complex](b.size)
    for(i <- 0 until b.size)
      x update(i, M(i, A.size))
    
    for(i <- 0 until b.size reverse) {
      for(j <- (i+1) until b.size)
        x update(i, x(i) - (M(i, j)*x(j)))
      x update(i, x(i) / M(i, i))
    }
    
    x
  }
}
