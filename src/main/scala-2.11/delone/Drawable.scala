package delone

import scalafx.scene.canvas.Canvas

/**
  * Created by Admin on 23.01.2017.
  */
trait Drawable {
  def draw(canvas: Canvas) :Unit
}
