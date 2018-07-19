package filterdesigner.ui.editor

import scala.collection.JavaConversions._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.stage.FileChooser
import scalafx.scene._
import scalafx.scene.layout._
import scalafx.scene.shape._
import scalafx.scene.control._
import scalafx.scene.paint._
import javafx.scene.input._
import javafx.event._
import scalafx.geometry._

trait Tool {
  def processMouse(e: MouseEvent): PartialFunction[EventType[MouseEvent], Unit]
  lazy val button = new ToggleButton {
    buttonShape.scaleX = 0.7
    buttonShape.scaleY = 0.7
    graphic = buttonShape
    toggleGroup = Tool.ToolGroup
    prefWidth = 55
    prefHeight = 55    
  }  
  protected val buttonShape: Shape
}

object Tool {
  val ToolGroup = new ToggleGroup
}

object Toolbar extends ToolBar {
  val tools = Vector(SelectionTool, WireTool, ResistorTool, CapacitorTool, InductorTool, OPATool, PotTool, GndTool)
  content = tools map {_ button}
  tools(0).button.selected = true  
  
  def currentTool = tools.find { _.button.selected.value }  
}

trait ReplaceCursor { this: Tool => 
  import CircuitDrawer._
  protected val mouseShape: ComponentShape
  protected def snap(x: Double, y: Double) = snapToGrid(x, y)
  def hideMouse = {
    group.children -= mouseShape
    scene.value.cursor = Cursor.DEFAULT    
  }
  final def processMouse(e: MouseEvent): PartialFunction[EventType[MouseEvent], Unit] = {    
    val rest = processMouse2(e)
    val func: PartialFunction[EventType[MouseEvent], Unit] = {
      case t @ MouseEvent.MOUSE_ENTERED =>              	
      	group.children += mouseShape
      	if(rest isDefinedAt t) rest(t)
      
      case t @ MouseEvent.MOUSE_EXITED =>
      	hideMouse
      	if(rest isDefinedAt t) rest(t)
      	
      case t @ (MouseEvent.MOUSE_MOVED | MouseEvent.MOUSE_DRAGGED) =>
        scene.value.cursor = Cursor.NONE
        if(! (group.children contains mouseShape))
          group.children += mouseShape
        val (x, y) = snap(e.sceneX, e.sceneY)        
        if(x < 0 || y < 0 || x > width.value || y > height.value) hideMouse
        else {
          mouseShape.layoutX = x
          mouseShape.layoutY = y
          if(! canPlace(mouseShape))
            mouseShape.stroke = Color.Red
          else
            mouseShape.stroke = Color.Grey          
        }
        if(rest isDefinedAt t) rest(t)
        
      case t if rest isDefinedAt t => rest(t)
    }
    func
  }
  protected def processMouse2(e: MouseEvent): PartialFunction[EventType[MouseEvent], Unit]
}
  


