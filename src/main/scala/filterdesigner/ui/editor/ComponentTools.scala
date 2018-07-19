package filterdesigner.ui.editor

import scalafx.Includes._
import scalafx.scene.input.MouseButton
import scalafx.scene.transform.Transform
import javafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.layout.Pane

sealed trait ComponentTool extends Tool with ReplaceCursor {
  import CircuitDrawer._
  protected def makeShape: ComponentShape  
  protected lazy val mouseShape = {
    val res = makeShape; res.stroke = Color.Grey; res
  }
  private var rotation = 0
  private def inCircuitShape = {
    val res = makeShape
    res.transforms += Transform.rotate(90*rotation, 0, 10)    
    res
  }
  private def rotate = {
    rotation = (rotation + 1) % 4
    mouseShape.transforms clear;
    mouseShape.transforms += Transform.rotate(90*rotation, 0, 10)
    ()    
  }
  protected val buttonShape = makeShape
  protected def processMouse2(e: MouseEvent) = { 
    case MouseEvent.MOUSE_CLICKED =>
      val (x, y) = snapToGrid(e.sceneX, e.sceneY)
      e.button match {
        case MouseButton.PRIMARY if canPlace(mouseShape) =>
          val newShape = inCircuitShape
          newShape.layoutX = x; newShape.layoutY = y
          Actions.act(new Action {
            def doo = {              
              group.children += newShape
              elements += newShape  
            }
            def undo = {
              group.children -= newShape
              elements -= newShape
            }
          })	  
        case MouseButton.SECONDARY =>
          rotate
        case _ => ()
      }
  }
}

case object ResistorTool extends ComponentTool	{protected def makeShape = new ResistorShape}
case object CapacitorTool extends ComponentTool	{protected def makeShape = new CapacitorShape}
case object InductorTool extends ComponentTool	{protected def makeShape = new InductorShape}
case object OPATool extends ComponentTool		{protected def makeShape = new OpAmpShape}
case object PotTool extends ComponentTool		{protected def makeShape = new PotentiometerShape}
case object GndTool extends ComponentTool		{protected def makeShape = new GroundShape}