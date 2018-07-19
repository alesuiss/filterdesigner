package filterdesigner.ui.editor

import filterdesigner.ui.simulator.Simulator
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.stage.FileChooser
import scalafx.scene._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.paint._
import scalafx.scene.input._
import scalafx.event._
import scalafx.geometry._
import javafx.event.EventHandler
import javafx.scene.input.KeyCombination

object SUIApp extends JFXApp {
  lazy val simulators = Buffer[Simulator]()
 
  stage = new JFXApp.PrimaryStage {
    title = "Filter Designer"
    width = 800
    height = 600
    
    val pane = new BorderPane {      
      top = new VBox { children = Seq(MainMenu, Toolbar) }
      bottom = new HBox        
      right = ControlBar
      center = CircuitDrawer
    }
    
    pane.prefWidth <== width
    pane.prefHeight <== height
    
    scene = new Scene {      
      fill = Color.White
      content = pane
      
      onKeyPressed = {e: KeyEvent => (e.code match {
        case KeyCode.S => Some(SelectionTool)
        case KeyCode.W => Some(WireTool)
        case KeyCode.R => Some(ResistorTool)
        case KeyCode.C => Some(CapacitorTool)
        case KeyCode.L => Some(InductorTool)
        case KeyCode.O => Some(OPATool)
        case KeyCode.P => Some(PotTool)
        case KeyCode.G => Some(GndTool)
        case _ => None
      }) foreach {x => x.button.selected = true ; e.consume} ; Toolbar.tools collect {case r: ReplaceCursor => r.hideMouse} }
      
      val undo = new KeyCodeCombination(KeyCode.Z, KeyCombination.CONTROL_DOWN)
      val redo = new KeyCodeCombination(KeyCode.Y, KeyCombination.CONTROL_DOWN)
      onKeyReleased = {e: KeyEvent =>
        if(undo `match` e) 
          Actions.undo
        else if(redo `match` e)
          Actions.redo
      }
    }    
  }  
}

object MainMenu extends MenuBar {
  useSystemMenuBar = true  
  private val chooser = new FileChooser {
    initialDirectory = new java.io.File(".").getCanonicalFile
    extensionFilters += new FileChooser.ExtensionFilter("Filter Designer circuits", "*.fd")
  }
  
  menus add new Menu("File") {
    items = Seq(
      new MenuItem("Load...") {
        onAction = { e: ActionEvent =>
          chooser.title = "Load circuit..."
          val file = chooser showOpenDialog scene.window.get
          if(file ne null)
            Sedes load file
        }
      }, new MenuItem("Save...") {
        onAction = { e: ActionEvent =>
          chooser.title = "Save circuit..."
          var file = chooser showSaveDialog scene.window.get
          if(file ne null) {
            if(!(file.getName endsWith ".fd"))
              file = new java.io.File(file.getAbsolutePath + ".fd")
            Sedes save file
          }
        }
      }      
    )            
  }.delegate
  
  menus add new Menu("Edit") {    
    items = Seq(
      new MenuItem("Undo") { 
        onAction = { e: ActionEvent => Actions.undo }
        disable = true
        Actions.canUndo onChange {disable = ! Actions.canUndo.value}
      },
      new MenuItem("Redo") {
        onAction = { e: ActionEvent => Actions.redo }
        disable = true
        Actions.canRedo onChange {disable = ! Actions.canRedo.value}
      }
    )
  }.delegate
  
  menus add new Menu("Simulation") {
    items = Seq(
      new MenuItem("Run") {
        onAction = { e: ActionEvent =>
          CircuitDrawer.buildCircuit match {
            case (a, b, c) => SUIApp.simulators += new Simulator(a, c, b)
          }
          ()
        }
      }      
    )            
  }.delegate
}

object ControlBar extends ScrollPane {
  val box = new VBox(24) {
    padding = Insets(10, 10, 10, 10)
  }
  content = box
  visible = false
  private def update = 
    visible = !box.children.isEmpty
   
  prefWidth = 260
  
  def add(c: Control) = c.controlPane foreach {pane => box.children += pane ; update}
  def remove(c: Control) = c.controlPane foreach {pane => box.children -= pane ; update}
  
  def clear = {box.children clear; update}
}

object Utils {
  implicit def funcToHandler[E <: javafx.event.Event](func: E => Unit): EventHandler[E] = new EventHandler[E] { def handle(e: E) = func(e) }
  def allChildren(p: javafx.scene.Parent): Seq[Node] = {
    val s = scala.collection.mutable.Stack[javafx.scene.Node](p)
    val children = scala.collection.mutable.Buffer[Node]()
    while(!s.isEmpty) {
      s pop match {   
        case p: javafx.scene.layout.Pane => s pushAll p.children 
        case n: javafx.scene.Node => children += n
      }
    }
    children
  }
  type CLFunc = PartialFunction[(_, _, _), Unit]
  implicit def makeChangeListener[T](f: CLFunc): javafx.beans.value.ChangeListener[T] = new javafx.beans.value.ChangeListener[T] {
    def changed(ov: javafx.beans.value.ObservableValue[_ <: T], oldval: T, newval: T) = {
      if(f isDefinedAt ((ov, oldval, newval))) f((ov, oldval, newval))
    }
  }
}

/**
 * 
 * // TODO: keyboard shortcuts
 * TODO: clipboard
 * // TODO: undo/redo
 * TODO: save as
 * TODO: phase response  
 * TODO: zoom?
 * TODO: pan 
 * TODO: exception catching
 * 
 */


