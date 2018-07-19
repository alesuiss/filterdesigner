package filterdesigner

trait Matrix {   
  val zero = RectComplex(0, 0)
  val rows: Int
  val cols: Int
  protected def get(row: Int, col: Int): Complex
  protected def set(row: Int, col: Int, v: Complex): Unit  
  
  override def toString: String = 0 until rows map {row =>
      0 until cols map {get(row, _)} mkString "\t\t\t\t\t"
  } mkString("\n")  
  
  def apply(row: Int, col: Int) = new ComplexProxy(get(row,col)) {
    def :=(c: Complex) = set(row, col, c)
    def +=(c: Complex) = set(row, col, if(value == zero) c else value + c)
    def -=(c: Complex) = set(row, col, if(value == zero) -c else value - c)
    def *=(c: Complex) = set(row, col, if(value == zero) 0 else value * c)
    def /=(c: Complex) = set(row, col, if(value == zero && c != zero) 0 else value / c)    
  }
}

trait SquareMatrix {
  val size: Int
  lazy val rows = size
  lazy val cols = size
}

trait DenseMatrixImpl extends Matrix {
  val array = new Array[Array[Complex]](rows)
  for(row <- 0 until rows) {
    array update(row, new Array[Complex](cols))
    0 until cols foreach {col => array(row) update(col, 0)}
  }    
  
  protected def get(row: Int, col: Int) = array(row)(col)
  protected def set(row: Int, col: Int, v: Complex) = array(row) update(col, v)      
}

class DenseMatrix(override val rows: Int, override val cols: Int) extends DenseMatrixImpl
class SquareDenseMatrix(val size: Int) extends SquareMatrix with DenseMatrixImpl
