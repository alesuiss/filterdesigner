package filterdesigner.ui.editor

import scalafx.Includes._
import javafx.scene.input.MouseEvent
import scalafx.scene.paint.Color

case object WireTool extends Tool with ReplaceCursor {
  import CircuitDrawer._
  protected val buttonShape = new WireIcon
  protected lazy val mouseShape = new Dot
  private var currentWire: Option[(Wire, Wire)] = None
  override protected def snap(x: Double, y: Double) = snapToLead(x, y)
  protected def processMouse2(e: MouseEvent) = { 
      
    case MouseEvent.MOUSE_DRAGGED =>      
      val (x1, y1) = snapToLead(dragMouseStart._1, dragMouseStart._2)
      val (x2, y2) = snapToLead(e.sceneX, e.sceneY)
      
      if(currentWire eq None) {
        val hwire = new HWire(x2 - x1) 
        hwire.layoutX = x1 ; hwire.layoutY = y1        
        val vwire = new VWire(y2 - y1) 
        vwire.layoutX = x2 ; vwire.layoutY = y1            
    	  currentWire = Some((hwire, vwire))
    	  group.children += hwire 
        group.children += vwire
      } else {
        currentWire.get._1.length = (x2 - x1)
        currentWire.get._2.layoutX = x2
        currentWire.get._2.length = (y2 - y1)
      }      
      if(canPlace(currentWire.get._1) && canPlace(currentWire.get._2)) {
        currentWire.get._1.stroke = Color.Grey; currentWire.get._2.stroke = Color.Grey
      } else {
        currentWire.get._1.stroke = Color.Red; currentWire.get._2.stroke = Color.Red
      }
      
    case MouseEvent.MOUSE_RELEASED =>
      if(dragging && (currentWire ne None)) {        
        val (x1, y1) = snapToGrid(dragMouseStart._1, dragMouseStart._2)      
        val (x2, y2) = snapToGrid(e.sceneX, e.sceneY)
        val wire = currentWire.get
        
        if(canPlace(wire._1) && canPlace(wire._2)) {
          wire._1.stroke = Color.Black; wire._2.stroke = Color.Black
          Actions.act(new Action {
            def doo = {
              wire._1 :: wire._2 :: Nil foreach {e =>
                if(e.length.abs > 0) elements += e else group.children -= e
              }
            }
            def undo = {
              wire._1 :: wire._2 :: Nil foreach {e =>
                if(e.length.abs > 0) {elements -= e ; group.children -= e}
              }
            }
            override def redo = {
              wire._1 :: wire._2 :: Nil foreach {e =>
                if(e.length.abs > 0) {elements += e ; group.children += e}
              }
            }
          })
        } else {
          group.children -= wire._1; group.children -= wire._2
        }
        currentWire = None
      }        
      
  }
}
