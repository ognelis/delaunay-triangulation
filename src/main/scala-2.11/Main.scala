/**
  * Created by Admin on 20.09.2016.
  */

import delone.Delone
import geometry.Vertex

import scala.annotation.tailrec
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane, GridPane, VBox}
import scalafx.scene.paint._

object Main extends JFXApp {

  val canvasWidth  = 1376
  val canvasHeight = 800
  val pointsNumber = 1000000

  val canva = new Canvas {
    width = canvasWidth
    height = canvasHeight
    val pixelDrawing = graphicsContext2D
    pixelDrawing.lineWidth = 1
    pixelDrawing.fill = Color.Black

    var points: ArrayBuffer[Vertex] = ArrayBuffer()
    onMouseClicked = (e: MouseEvent) => {
      points += Vertex(e.x, canvasHeight - e.y)
      pixelDrawing.fillOval(e.x, e.y, 5, 5)
    }
  }

  val npoint = new TextField{}
  val randomPoint = new Button("randomize pixels") {
    var pixels: Array[Vertex] = Array()
    onMouseClicked = (e: MouseEvent) => Future {
      canva.pixelDrawing.clearRect(0,0,canvasWidth,canvasHeight)
      pixels = (1 to npoint.text.value.toInt).toArray.map(x => Vertex(Random.nextInt(canvasWidth).toDouble + Random.nextDouble(), canvasHeight.toDouble - Random.nextInt(canvasHeight).toDouble + Random.nextDouble())).distinct
//      canva.pixelDrawing.fill = Color.Red
//      pixels foreach (pixel => canva.pixelDrawing.fillOval(pixel.x,pixel.y,5,5))
      canva.pixelDrawing.fill = Color.Black
    }
  }



  val computeButton = new Button("compute") {
    onMouseClicked = (e: MouseEvent) => {
      val delone =  new Delone((canva.points ++= randomPoint.pixels).distinct)

      delone.buildAdjacentTriangles

      delone.triangles.combinations(2).foreach(x => delone.angleProtiv(x(0), x(1)))
      delone.deloneAll()

      println("It' okay")
      delone.triangles.filter(_.isDrawable) foreach (x => x.draw(canva))
    }
  }

  val clearButton = new Button("clear") {
    onMouseClicked = (e: MouseEvent) => {
      canva.points = ArrayBuffer.empty
      randomPoint.pixels = Array.empty
      canva.pixelDrawing.clearRect(0,0,canvasWidth,canvasHeight)
    }
  }

  val settings = new VBox {
    style = "-fx-background-color: blue"
    children = Seq(randomPoint,npoint,computeButton,clearButton)
  }

  stage = new PrimaryStage {
    title = "Graham"
    scene = new Scene {
      root = new BorderPane {
        left = settings
        center = new GridPane{
          fill = Color.rgb(38, 38, 38)
          children = canva
        }
      }
    }
  }
}
