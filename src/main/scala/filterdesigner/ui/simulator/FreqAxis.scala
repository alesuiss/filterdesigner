package filterdesigner.ui.simulator

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import scalafx.scene.chart._

private class JavaFreqAxis extends javafx.scene.chart.ValueAxis[java.lang.Double] {
  setLabel("Frequency (Hz)")
  private def log10(d: Double) = Math.log(d) / Math.log(10)
  
  var length: Double = 0
  
  override protected def getTickMarkLabel(f: java.lang.Double) = {
    val s = f.toInt.toString
    s.head match {
      case '1' | '2' => s.size match {
        case i if i >= 10 => (f / 1000000000.0).toInt.toString + "G"
        case i if i >= 7 => (f / 1000000.0).toInt.toString + "M"
        case i if i >= 4 => (f / 1000.0).toInt.toString + "k"
        case _ => s
      }
        
      case _ => ""
    }
  }
  
  override protected def autoRange(min: Double, max: Double, len: Double, lsize: Double) = {    
    Array(new java.lang.Double(min), new java.lang.Double(max))
  }
  
  val allticks = Seq(10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000) flatMap {1 to 10 map _.*} map {new java.lang.Double(_)}
  override def calculateTickValues(len: Double, range: Any) = {
    val rr = range.asInstanceOf[Array[java.lang.Double]]
    val (lo, hi) = (rr(0), rr(1))
    length = len
    allticks filter {t => t >= lo && t <= hi}
  }
  this setAutoRanging true
  private val range = Array[java.lang.Double](10, 10000)  
  
  override def getDisplayPosition(i: java.lang.Double) = {
    val delta = log10(range(1)) - log10(range(0))
    val deltav = log10(i) - log10(range(0))
    (deltav / delta) * length
  }  
  
  /*def getActualPosition(d: Double) = {
    Math.pow(range(1)/range(0), d/length) * range(0)
  } */ 
  
  override def getRange = range.clone
  override protected def setRange(r: Any, a: Boolean) = {
    val rr = r.asInstanceOf[Array[java.lang.Double]]
    range update(0, rr(0))
    range update(1, rr(1))
  }
  
  override def calculateMinorTickMarks = Nil
}

class FrequencyAxis extends ValueAxis[java.lang.Double](new JavaFreqAxis) {
  /*def getFreqs = {
    val axis = delegate.asInstanceOf[JavaFreqAxis]
    val range = axis.getRange
    (range(0) +: (1 to (axis.length.toInt-1) by 4 map {x => Math.pow(range(1)/range(0), x.toDouble/axis.length) * range(0)})) :+ range(1)
  }*/
}
