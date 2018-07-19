package filterdesigner
package ui
package editor

import scala.collection.mutable.{Buffer, HashMap, HashSet, Stack}

import scalafx.Includes._
import javafx.event.EventType
import scalafx.geometry.Point2D
import javafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.{Group, Node}

import Net._

object CircuitDrawer extends Pane {
  import Utils._  
  
  val group = new Group
  children += group
  
  var dragMouseStart = (0.0, 0.0)
  var dragging = false
  
  private val toolHandler = {e: MouseEvent => Toolbar.currentTool foreach { tool =>
    val func = tool processMouse e
    val typ = e.getEventType.asInstanceOf[EventType[MouseEvent]]
    if(func isDefinedAt typ)
      func(typ)
  }}
  onMouseClicked = toolHandler  
  onMouseEntered = toolHandler
  onMouseExited = toolHandler
  onMouseMoved = toolHandler
  onMouseDragged = {e: MouseEvent => dragging = true; e} andThen toolHandler
  onMousePressed = {e: MouseEvent => dragMouseStart = (e.sceneX, e.sceneY); e} andThen toolHandler
  onMouseReleased = toolHandler andThen {_ => dragging = false}
  
  val gridStep = 5
  def snapToGrid(x: Double, y: Double) = {
    val xy = sceneToLocal(x, y)    
    val (xres, yres) = (Math.floor(xy.x / gridStep) * gridStep, Math.floor(xy.y / gridStep) * gridStep)   
    (xres.toInt, yres.toInt)
  }
  val leadSnapDist = 20
  def snapToLead(x: Double, y: Double) = {
    val (xg, yg) = snapToGrid(x, y)
    def pos = new Point2D(xg, yg)
    val closer = new Ordering[Point2D] {
      def compare(p1: Point2D, p2: Point2D) = {
        ((p1 distance pos) - (p2 distance pos)) toInt
      }
    }
    elements flatMap {e => e.leads map {case (x, y) => new Point2D(x, y)} map {p2d => new Point2D(e localToParent p2d)}} min closer match {
      case p if (p distance pos) < leadSnapDist => (p.x.toInt, p.y.toInt)
      case _ => (xg.toInt, yg.toInt)
    }
  } 
  
  val elements = Buffer[ComponentShape](new Input, new Output)
  
  elements foreach {group.children += _}
  def elementUnder(x: Double, y: Double): Option[ComponentShape] =     
    elements find {_.getBoundsInParent contains(x, y)}  
  
  def canPlace(s: ComponentShape) = {
    (elements-s) filter {s.getBoundsInParent intersects _.getBoundsInParent} forall {areConnected(_, s)}      
  }  
  
  private def areConnected(e1: ComponentShape, e2: ComponentShape) = {
    val e1leads = e1.leads map {case ((x, y)) => val xy = e1 localToParent(x, y); (xy.x.toInt, xy.y.toInt)}      
    val e2leads = e2.leads map {case ((x, y)) => val xy = e2 localToParent(x, y); (xy.x.toInt, xy.y.toInt)}
    (e1leads count e2leads.contains) == 1
  }
  
  def buildCircuit = {
    val wires = elements collect {case w: Wire => w}
    val netnums = HashMap[(Double, Double), Int]()
    def numberNet(num: Int, lead: (Double, Double)) = {
      netnums += ((lead, num))
      val closed = HashSet[Wire]()      
      val stack = Stack[Wire](wires filter {w =>
        val xy = w.parentToLocal(lead._1, lead._2)
        w.contains(xy.x, xy.y)
      }: _*)      
      while(! stack.isEmpty) {
        val w = stack pop;        
        netnums ++= w.leadsInParent map {(_, num)}
        closed += w
        for(w2 <- wires if !closed(w2) && areConnected(w, w2))
          stack push w2
      }      
    }
    
    // 1. Number leads of all ground-wires -1    
    elements collect {case g: GroundShape => g} flatMap {_ leadsInParent} foreach {numberNet(-1, _)}   
    // 2. Number all other leads
    var n: Int = 0
    elements flatMap {_ leadsInParent} foreach {lead =>
      if(!(netnums contains lead))
        {numberNet(n, lead); n += 1}
    }
    
    // 3. Make netlist
    val variables = elements map {_ control} collect {case v: Variable => v.label}
    val inputNet = filterdesigner.Net(netnums(elements(0).leadsInParent(0)))
    val outputNet = filterdesigner.Net(netnums(elements(1).leadsInParent(0)))
    val circuit = new filterdesigner.Circuit {
      val nbNets = n
      val nets = (elements -- wires map {comp =>
        comp.control makeNetlist (comp.leadsInParent map netnums map {case -1 => Ground; case i => filterdesigner.Net(i)})
      } reduceLeft {_ ++ _}) + ((new filterdesigner.Resistor(100000000), IndexedSeq(outputNet, Ground))) +
      						             ((new filterdesigner.Resistor(100000000), IndexedSeq(inputNet, Ground)))
    }    
    
    (circuit, variables.toSeq, outputNet)
  }
  
}


