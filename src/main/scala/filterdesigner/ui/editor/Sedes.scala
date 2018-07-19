package filterdesigner.ui.editor

import scala.collection.JavaConversions._
import scalafx.Includes._
import scalafx.scene.transform._
import java.io.File
import java.io.DataOutputStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.DataInputStream
import java.io.BufferedInputStream
import java.io.FileInputStream

object Sedes {
  import CircuitDrawer._
  
  def save(file: File) = {
    val out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
    out writeInt elements.size
    elements foreach {e =>
      out writeUTF e.getClass.getCanonicalName
      e match {        
        case w: Wire => out writeInt w.length
        case _ => ()
      }
      out writeInt e.layoutX.toInt; out writeInt e.layoutY.toInt
      e.transforms find {_.isInstanceOf[javafx.scene.transform.Rotate]} match {
        case Some(rot: javafx.scene.transform.Rotate) => 
          out writeInt rot.getAngle.toInt
          out writeInt rot.getPivotX.toInt
          out writeInt rot.getPivotY.toInt
        case _ =>
          out writeInt -1
      }
      e.control save out      
    }
    out close
  }
  
  def load(file: File) = {
    elements clear;
    group.children clear;
    SelectionTool.selection clear;
    val in = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))
    val num = in.readInt
    for(_ <- 1 to num) {
      val clasz = Class forName in.readUTF
      val e = clasz match {
        case x if x == classOf[HWire] => new HWire(in.readInt)
        case x if x == classOf[VWire] => new VWire(in.readInt)
        case _ => clasz.newInstance.asInstanceOf[ComponentShape]
      }
      val (x, y) = (in readInt, in readInt)
      val angle = in readInt;
      e.transforms clear;
      if(angle != -1) 
        e.transforms += Transform.rotate(angle, in readInt, in readInt)
        
      e.control load in
      elements += e
      e.layoutX = x; e.layoutY = y
      group.children += e      
    }
    
    in close
  }
}