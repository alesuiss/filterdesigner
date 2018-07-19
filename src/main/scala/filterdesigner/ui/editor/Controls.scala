package filterdesigner
package ui
package editor

import scala.collection.JavaConversions._
import java.io.{DataOutputStream, DataInputStream}
import scalafx.Includes._
import scalafx.scene.layout.{Pane, HBox, VBox}
import scalafx.scene.control.{Label, TextField, RadioButton, CheckBox, ToggleGroup}

import Net._

trait Control {
  def makeNetlist(nets: IndexedSeq[Net]): Map[Component, IndexedSeq[Net]]
  val controlPane: Option[Pane]
  def save(out: DataOutputStream): Unit = ()
  def load(in: DataInputStream): Unit = ()
}

trait Variable { this: Control =>
  def label: String
}

trait SimpleValuePane { this: Control =>
  def labelText: String
  def defaultVal: String
  val valueParser: String => ComplexExpression
  val valuetxt = new TextField { text = defaultVal }
  def value = valueParser(valuetxt.text.value)
  val controlPane = Some(new VBox(8) {
    children = Seq(
        new Label(labelText) {
          underline = true
          style = "-fx-font-weight: bold;"
        },
        new HBox(8) {
          children = Seq(new Label("Value:"), valuetxt)
        }
    )     
  })
  override def save(out: DataOutputStream) = out writeUTF valuetxt.text.value
  override def load(in: DataInputStream) = valuetxt.text = in.readUTF
}

object ResistorParser extends (String => ComplexExpression) {
  def apply(v: String) = {
    val (v1, v2) = v splitAt (v.size - 1)
    Constant(v2(0) match {
      case 'r' | 'R' => v1.toDouble
      case 'k' | 'K' => 1000.0 * v1.toDouble
      case 'm' | 'M' => 1000000.0 * v1.toDouble
      case c if c >= '0' && c <= '9' => v.toDouble
    })
  }
}

class ResistorControl extends Control with SimpleValuePane {
  def labelText = "Resistor"
  def defaultVal = "1k"
  val valueParser = ResistorParser
  def makeNetlist(nets: IndexedSeq[Net]) = Map((new Resistor(value), nets))
  
}

class CapacitorControl extends Control with SimpleValuePane {
  def labelText = "Capacitor"
  def defaultVal = "1µ"
  val valueParser = { v: String =>
    val (v1, v2) = v splitAt (v.size - 1)
    Constant(v1.toDouble / (v2(0) match {
      case 'm' 			=> 1000.0
      case 'u' | 'µ' 	=> 1000000.0
      case 'n'		 	=> 1000000000.0
      case 'p'		 	=> 1000000000000.0
    }))
  }
  def makeNetlist(nets: IndexedSeq[Net]) = Map((new Capacitor(value), nets))  
}

class InductorControl extends Control with SimpleValuePane {
  def labelText = "Inductor"
  def defaultVal = "100m"
  val valueParser = { v: String =>
    val (v1, v2) = v splitAt (v.size - 1)
    Constant(v1.toDouble / (v2(0) match { 
      case 'H'			=> 1.0
      case 'm' 			=> 1000.0
      case 'u' | 'µ' 	=> 1000000.0
      case 'n'		 	=> 1000000000.0
      case 'p'		 	=> 1000000000000.0
      case c if c >= '0' && c <= '9' => 10.0 * (if(v1.size > 0) v1.toDouble else 0) + (c - '0')
    }))
  }
  def makeNetlist(nets: IndexedSeq[Net]) = Map((new Inductor(value), nets))  
}

class PotentiometerControl extends Control with Variable {
  def label = lblbox.text.value
  def makeNetlist(nets: IndexedSeq[Net]) = {
    val r = ResistorParser(valbox.text.value)  
    val base: ComplexExpression = 10
    val x: ComplexExpression = if(! reverse.isSelected) lblbox.text.value else Constant(1) - lblbox.text.value
    val rbot = if(linlog.lin.selected.value) r * x else (r / base) * (base ** x)
    val rtop = r - rbot    
    Map((new Resistor(rtop), Array(nets(0), nets(1))), (new Resistor(rbot), Array(nets(1), nets(2))))      
  }
  
  val lblbox = new TextField {text = PotentiometerControl.makeLabel}
  val valbox = new TextField {text = "10k"}
  val reverse = new CheckBox("reverse")
  
  object linlog extends ToggleGroup {
    lazy val lin = new RadioButton("lin") { selected = true }
    lazy val log = new RadioButton("log")
    toggles = Seq(lin, log)
  } 
 
  override val controlPane = Some(new VBox(8) {
    children = Seq(
      new Label("Potentiometer") {underline = true ; style = "-fx-font-weight: bold;"},
      new HBox(8) {
        children = Seq(
          new Label("Label:"),
          lblbox
        )
      },
      new HBox(8) {
        children = Seq(
          new Label("Value:"),
          valbox
        )
      },
      new HBox(8) { children = linlog.lin :: linlog.log :: Nil },
      new HBox(8) { children += reverse }
    )
  })
  
  override def save(out: DataOutputStream) = {
    out writeUTF lblbox.text.value
    out writeUTF valbox.text.value
    out writeBoolean linlog.lin.selected.value
    out writeBoolean reverse.selected.value
  }
  override def load(in: DataInputStream) = {
    lblbox.text = in.readUTF
    valbox.text = in.readUTF
    linlog.lin.selected = in.readBoolean
    linlog.log.selected = !(linlog.lin.selected.value)
    reverse.selected = in.readBoolean
  }
}

object PotentiometerControl {
  private var last = 0
  def makeLabel = {
    last += 1
    "P" + last
  }
} 

class OPAControl extends Control {
  def makeNetlist(nets: IndexedSeq[Net]) = Map((new OpAmp, nets))
  val controlPane = None
}

object InputControl extends Control {
  def makeNetlist(nets: IndexedSeq[Net]) = Map((new Source, Array(nets(0), Ground)))
  val controlPane = None
}

trait NoControl extends Control {
  def makeNetlist(nets: IndexedSeq[Net]) = Map()
  val controlPane = None
}
object OutputControl extends NoControl
object GroundControl extends NoControl 
object WireControl extends NoControl
