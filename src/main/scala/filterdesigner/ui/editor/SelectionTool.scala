package filterdesigner.ui.editor

import scala.collection.mutable.{HashSet}
import scalafx.Includes._
import javafx.scene.input.{MouseEvent, MouseButton}
import scalafx.scene.effect.DropShadow
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.control._
import scalafx.event._

case object SelectionTool extends Tool {
  import CircuitDrawer._ // fuck encapsulation
  import Utils._
  protected val buttonShape = new ArrowShape
  private var dragStart: Map[ComponentShape, (Double, Double)] = Map()
  val selection = new HashSet[ComponentShape] {
    override def clear = {this foreach {_.effect = null}; ControlBar clear; super.clear}
    override def remove(e: ComponentShape) = {e.effect = null; ControlBar remove e.control; super.remove(e)}
    override def -=(e: ComponentShape) = {e.effect = null; ControlBar remove e.control; super.-=(e)}
    override def +=(e: ComponentShape) = {e.effect = selectedEffect; ControlBar add e.control; super.+=(e)}
    override def add(e: ComponentShape) = {e.effect = selectedEffect; ControlBar add e.control; super.add(e)}
  }
  val selectedEffect = new DropShadow {
    color = Color.Orange
    spread = 0.5
  }  
  val hoverEffect = new DropShadow {
    color = Color.Yellow
    spread = 0.5
  }    
  
  val menu = new ContextMenu
  val delete = new MenuItem("Delete")
  delete.onAction = {e: ActionEvent => selection foreach {e => elements -= e; group.children -= e}; selection clear}
  menu.items += delete  
  
  def processMouse(e: MouseEvent) = {      
    
    case MouseEvent.MOUSE_PRESSED => 
      val (x, y) = snapToGrid(e.sceneX, e.sceneY)            
      elementUnder(x, y) match {
        case Some(elm) if ! selection(elm) => selection clear; selection += elm
        case None => selection clear
        case _ => ()
      }
      if(! selection.isEmpty) {
        val selseq = selection.toSeq
    	  dragStart = Map(selseq zip (selseq map {e => (e.getLayoutX, e.getLayoutY)}): _*)        
      } else {
        selectionSquare = Some(new Rectangle {   //FIXME this looks fucked
          x = -100 ; y = -100
          width = 0 ; height = 0
          fill = Color.LightSteelBlue
          stroke = Color.DarkBlue
          opacity = 0.5
        })
        group.children += selectionSquare.get
      }
      delete.disable = selection.isEmpty || (selection contains elements(0)) || (selection contains elements(1))
        
    case MouseEvent.MOUSE_RELEASED =>      
      selectionSquare match {
        case None =>    
          if(dragging && !selection.isEmpty && ! selection.forall(canPlace))
    	    selection foreach {e => dragStart(e) match {case (x, y) => e.layoutX = x; e.layoutY = y}; e.stroke = Color.Black}
      
        case Some(rect) => 
          group.children -= rect
          selectionSquare = None
      }
      delete.disable = selection.isEmpty || (selection contains elements(0)) || (selection contains elements(1))      
      selection foreach {_.effect = selectedEffect}
      if(!dragging && e.button == MouseButton.SECONDARY) {        
        menu.show(CircuitDrawer, e.screenX, e.screenY)
      } else menu.hide
      
        
    case MouseEvent.MOUSE_MOVED =>         
      val (x, y) = snapToGrid(e.sceneX, e.sceneY)
      (elements -- selection) foreach {_ .effect = null}
      elementUnder(x, y) match {case Some(elm) if ! selection(elm) => elm.effect = hoverEffect; case _ => ()}
        
    case MouseEvent.MOUSE_DRAGGED =>
      val (dx, dy) = (e.sceneX - dragMouseStart._1, e.sceneY - dragMouseStart._2)
      
      selectionSquare match {
        case None => 
          selection foreach {e =>
      		  val xy = localToScene(dragStart(e)._1 + dx, dragStart(e)._2 + dy)
      		  val (x, y) = snapToGrid(xy.x, xy.y)      
      		  e.layoutX = x; e.layoutY = y
      		  e.effect = null
          }
          if(!(selection forall canPlace))
        	  selection foreach {_.stroke = Color.Red}
          else
        	  selection foreach {_.stroke = Color.Black}      
      
        case Some(rect) =>
          val xy = sceneToLocal(dragMouseStart._1, dragMouseStart._2)
          rect.x = (if(dx < 0) xy.x + dx else xy.x)
          rect.y = (if(dy < 0) xy.y + dy else xy.y)            
          rect.width = dx.abs; rect.height = dy.abs
          selection clear;
          selection ++= elements filter {rect.getBoundsInParent contains _.getBoundsInParent}
      }
      
      delete.disable = selection.isEmpty || (selection contains elements(0)) || (selection contains elements(1)) 
  }
  
  private var selectionSquare: Option[Rectangle] = None
}  
