package filterdesigner.ui.simulator

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.Platform
import scalafx.stage.Stage
import scalafx.scene._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.paint._
import scalafx.scene.shape._
import scalafx.scene.chart._
import scalafx.event._
import scalafx.geometry._

import filterdesigner.{Circuit, Net, NodeAnalysis, Constant, RectComplex}

class Simulator(val circuit: Circuit, val outputNet: Net, val variables: Seq[String]) extends Stage {  
  import filterdesigner.ui.editor.Utils.funcToHandler
  def log10(d: Double) = Math.log(d) / Math.log(10)  

  val gainAxis = new NumberAxis("Gain (dB)", -40, 20, 5)
  val freqAxis = new FrequencyAxis 
  val freqSliders = new MinMaxPair(log10(2), log10(1000000000), log10(20), log10(10000), freqAxis)
  val gainSliders = new MinMaxPair(-140, 140, -40, 20, gainAxis) {
    sl foreach {_ setOrientation Orientation.VERTICAL}
  }    
  val chart = new LineChart[java.lang.Double, Number](freqAxis, gainAxis) {
    createSymbols = false
    animated = false
    vgrow = Priority.Always
  }    
  
  val sliders: Map[String, Slider] = Map(variables map {(_, new Slider(0.000000001, 0.999999999, 0.5) {
    value onChange calc
  })}: _*)      
  
  freqAxis.width onChange {freqSliders.sl foreach {_.prefWidth = freqAxis.width.value/2}}
  gainAxis.height onChange {gainSliders.sl foreach {_.prefHeight = gainAxis.height.value/2}}  
  
  val pane = new BorderPane {    
    prefWidth = 1000
    prefHeight = 600    
    
    left = new VBox(8) { 
      padding = Insets(10, 10, 10, 10)
      alignment = Pos.TopCenter
      children = gainSliders.sl.reverse
    }    
    right = new VBox(8) { 
      padding = Insets(10, 10, 10, 10)
      alignment = Pos.TopLeft
      children = sliders map {case (lbl, slider) => new VBox(8) {
        alignment = Pos.TopLeft
        children += Label(lbl + ":")
        children += slider
      }}
    }
    center = new VBox {    
      children += chart      
    }    
    top = new HBox(8) {
      padding = Insets(10, 10, 10, 10)
      alignment = Pos.BottomLeft
      children += Rectangle(center.value.layoutX.value + 50, 10, Color.Transparent)      
      freqSliders.sl foreach {children += _}
    }
  }
  
  scene = new Scene {    
    fill = Color.White
    content = pane   
    Platform runLater {
      pane.prefWidth bind width
      pane.prefHeight bind height
    }
  }  
  show  
  
  def calc: Unit = {
    val gainData = new XYChart.Series[java.lang.Double, Number] {
      name = "Gain (dB)"
    }    
    
    val params = sliders map {case (lbl, slider) => (lbl, RectComplex(slider.getValue, 0))}       
    
    (freqs map {f =>
      val resp = new NodeAnalysis(circuit, params ++ Map("f" -> RectComplex(f, 0))) getVoltage outputNet ;      
      (f, 20.0 * log10(resp.rho))      
    }).seq foreach {
      case (f, gain) =>
        gainData.getData add XYChart.Data[java.lang.Double, Number](f, gain)
    }    
    chart.data = gainData
  }
  
  var freqs: scala.collection.GenSeq[Double] = null
  def makeFreqs = {
    val buf = Buffer[Double]()
    var (f, hi) = (freqAxis.getLowerBound, freqAxis.getUpperBound)
    while(f <= hi) {
      buf += f
      f *= 1.05
    }
    freqs = Vector(buf: _*).par
  }
  
  makeFreqs
  calc  
  
  protected class MinMaxPair(val min: Double, val max: Double, val startMin: Double, val startMax: Double, target: ValueAxis[_]) {
    private val mindiff = if(target eq freqAxis) log10(5) else 5
    val slMin: Slider = new Slider(min, startMax - mindiff, startMin) {
      value onChange {slMax.min = value.value + mindiff ; update}
    }
    val slMax: Slider = new Slider(startMin + mindiff, max, startMax) {
      value onChange {slMin.max = value.value - mindiff ; update}
    }
        
    val sl = Seq(slMin, slMax)
    def update = {
      var (lo, hi) = (slMin.value.value, slMax.value.value)
      if(target eq freqAxis) {
        lo = Math.pow(10, lo)
        hi = Math.pow(10, hi)
      }
      target.lowerBound = lo.toInt
      target.upperBound = hi.toInt
      if(target eq freqAxis) {        
        makeFreqs
        calc
      }
    }    
    
    var (lo, hi) = (startMin, startMax)
    if(target eq freqAxis) {
      lo = Math.pow(10, lo).toInt
      hi = Math.pow(10, hi).toInt
    }    
    target.lowerBound = lo.toInt
    target.upperBound = hi.toInt        
  }
}

