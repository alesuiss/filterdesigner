package filterdesigner.ui.editor

import scala.collection.JavaConversions._

import scalafx.Includes._
import scalafx.scene.paint.Color
import scalafx.scene.shape._
import scalafx.scene.effect.{DropShadow, Effect}
import scalafx.scene.transform.Transform

sealed trait ComponentShape extends Shape {
  def leads: IndexedSeq[(Double, Double)]
  def leadsInParent = leads map {case (x, y) =>
    val xy = localToParent(x, y); (xy.x, xy.y)
  }
  protected def makeControl: Control
  
  val ctlFocusEffect = new DropShadow {
    color = Color.Red
    spread = 0.5
  }  
  
  lazy val control = {
    import Utils._
    val result = makeControl
    var oldeffect: Effect = null
    result.controlPane foreach {Utils allChildren _ foreach {_.focusedProperty addListener ({
      case (_, _, true) => oldeffect = effect.value; effect = ctlFocusEffect
      case (_, _, false) => effect = oldeffect      
    }: CLFunc)}}
    result
  }
}

class ResistorShape extends Path with ComponentShape {
  val rpoints: Seq[(Double, Double)] = Seq((10, 10), 
										   (12.5, 5),
										   (17.5, 15),
										   (22.5, 5),
										   (27.5, 15),
										   (32.5, 5),
										   (37.5, 15),
										   (40, 10),										   
										   (50, 10))
  
  elements = MoveTo(0, 10) +: (rpoints map {case (x, y) => LineTo(x, y)})    
  
  val leads: IndexedSeq[(Double, Double)] = IndexedSeq((0, 10), (50, 10))
  protected def makeControl: Control = new ResistorControl
}

class PotentiometerShape extends ResistorShape {
  elements ++= Seq(MoveTo(20, 0), LineTo(25, 5), LineTo(30, 0), MoveTo(25, 5), LineTo(25, -5))  
  override val leads = IndexedSeq((0.0, 10.0), (25.0, -5.0), (50.0, 10.0))
  override protected def makeControl = new PotentiometerControl
}

class CapacitorShape extends Path with ComponentShape {
  elements = Seq(
      MoveTo(0, 10), LineTo(22, 10), MoveTo(22, 2), LineTo(22, 18),
      MoveTo(28, 2), LineTo(28, 18), MoveTo(28, 10), LineTo(50, 10)
  )
  val leads: IndexedSeq[(Double, Double)] = IndexedSeq((0, 10), (50, 10))
  protected def makeControl = new CapacitorControl
}

class InductorShape extends Path with ComponentShape {
  elements = Seq(
      MoveTo(0, 10), LineTo(10, 10), QuadCurveTo(15, 0, 20, 10), QuadCurveTo(25, 0, 30, 10),
      QuadCurveTo(35, 0, 40, 10), LineTo(50, 10)
  )
  val leads = IndexedSeq((0.0, 10.0), (50.0, 10.0))
  protected def makeControl = new InductorControl
}

class OpAmpShape extends Path with ComponentShape {
  elements = Seq (
    MoveTo(0, 10), LineTo(10, 10),
    MoveTo(10, 0), LineTo(42.5, 20), LineTo(10, 40), LineTo(10, 0),
    MoveTo(0, 30), LineTo(10, 30),
    MoveTo(42.5, 20), LineTo(50, 20)
  )
  val leads: IndexedSeq[(Double, Double)] = IndexedSeq((0, 10), (0, 30), (50, 20))
  protected def makeControl = new OPAControl
}

class ArrowShape extends Polygon {
  points ++= Seq((4, 0),
			           (8, 6),
			           (5, 5),
			           (5, 8),
			           (3, 8),
			           (3, 5),
			           (0, 6),
			           (4, 0)) flatMap {case (x, y) => Array(x * 1.75, y*3)} map {new java.lang.Double(_)} 
  
  fill = Color.White
  stroke = Color.Black
  rotate = -30
}

class GroundShape extends Path with ComponentShape {
  elements = Seq(
      MoveTo(10, 0), LineTo(10, 4),
      MoveTo(2.5, 5), LineTo(17.5, 5),
      MoveTo(5, 8), LineTo(15, 8),
      MoveTo(7.5, 11), LineTo(12.5, 11),
      MoveTo(9.5, 14), LineTo(10.5, 14)
  )
  val leads = IndexedSeq((10.0, 0.0))
  protected val makeControl = GroundControl
}

class Dot extends Circle with ComponentShape {
  centerX = 0 ; centerY = 0 ; radius = 3
  fill = Color.Blue
  val leads = IndexedSeq((0.0, 0.0))
  protected def makeControl = null
}

class WireIcon extends Line {
    startX = 0 ; startY = 0
    endX = 20 ; endY = 0
}

trait Wire extends Line with ComponentShape{
  def length: Int
  def length_=(l: Int): Unit
  protected val makeControl = WireControl
}

class HWire(var len: Double) extends Line with ComponentShape with Wire { 
  startX = 0 ; startY = 0
  endX = len ; endY = 0
  def leads = IndexedSeq((0.0, 0.0), (len, 0.0))
  def length_=(l: Int) = {endX = l; len = l}
  def length = len.toInt
}

class VWire(var len: Double) extends Line with ComponentShape with Wire {
  startX = 0 ; startY = 0
  endX = 0 ; endY = len
  def leads = IndexedSeq((0.0, 0.0), (0.0, len))
  def length_=(l: Int) = {endY = l; len = l}
  def length = len.toInt
}

class Input extends Path with ComponentShape {
  layoutX = 30 ; layoutY = 100
  elements = Seq(
      MoveTo(0, 0), LineTo(30, 0), LineTo(43, 10), LineTo(30, 20), LineTo(0, 20), LineTo(0, 0),
      MoveTo(43, 10), LineTo(50, 10)
  )
  val leads = IndexedSeq((50.0, 10.0))
  protected val makeControl: Control = InputControl
}

class Output extends Input {
  layoutX = 400 ; layoutY = 100
  transforms += Transform.rotate(180, leads(0)._1, leads(0)._2)
  override protected val makeControl = OutputControl
}
