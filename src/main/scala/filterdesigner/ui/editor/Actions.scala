package filterdesigner
package ui
package editor

import scalafx.Includes._
import scalafx.beans.property._

trait Action {
  def doo: Unit
  def undo: Unit
  def redo: Unit = doo
}

object Actions {
  private val _thingsDone = new collection.mutable.ArrayStack[Action]
  private val _thingsUndone = new collection.mutable.ArrayStack[Action]
  private val _canUndo = new BooleanProperty
  private val _canRedo = new BooleanProperty
  
  def act(task: Action) = {
    _thingsUndone.clear    
    task.doo    
    _thingsDone += task
    fixprops
  }
  
  def undo = if(canUndo.value) {
    val task = _thingsDone.pop
    task.undo    
    _thingsUndone += task
    fixprops
  }
  
  def redo = if(canRedo.value) {
    val task = _thingsUndone.pop
    task.redo
    _thingsDone += task
    fixprops
  }
  
  private def fixprops = {
    _canUndo.value = ! _thingsDone.isEmpty
    _canRedo.value = ! _thingsUndone.isEmpty
  }
  
  def canUndo: ReadOnlyBooleanProperty = _canUndo
  def canRedo: ReadOnlyBooleanProperty = _canRedo
}