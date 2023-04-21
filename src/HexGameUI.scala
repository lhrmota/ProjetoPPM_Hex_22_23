import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.canvas.GraphicsContext
import javafx.scene.input.MouseEvent
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.stage.Stage

class HexGameUI extends Application {
  val canvasWidth = 1000
  val canvasHeight = 600
  val hexRadius=50
  val boardDimension=7

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Hex Game")
    val root = new Pane
    val canvas = new Canvas(canvasWidth, canvasHeight)
    root.getChildren.add(canvas)
    val mainScene=new Scene(root)
    primaryStage.setScene(mainScene)
    primaryStage.show()

    val gc = canvas.getGraphicsContext2D
    // Draw the hexagons
    for( line <- 0 until boardDimension)
      for( column<- 0 until boardDimension)
      drawHexagon(gc, (column+1+line*.5)*hexRadius*1.75, (line+1)*hexRadius*1.5, hexRadius, Color.RED)
    //drawHexagon(gc, 135, 50, hexRadius, Color.BLUE)
    //drawHexagon(gc, 93, 125, hexRadius, Color.GREEN)


    canvas.setOnMouseClicked(event => {
        //println("Clicked on canvas"+event)
        val line=Math.round((event.getY-hexRadius)/hexRadius/1.5)
        val rowRaw=(event.getX-hexRadius)/hexRadius/1.75
      val row=Math.round(rowRaw-(line*.5))
        println("Clicked hex row:"+row+" line:"+line)

    })
  }

  // Draw a hexagon at the given (x, y) coordinates with the given radius and color
  def drawHexagon(gc: GraphicsContext, x: Double, y: Double, radius: Double, color: Color): Unit = {
    gc.setFill(color)
    gc.setStroke(Color.BLACK)
    gc.setLineWidth(2)
    val points = calculateHexagonPoints(x, y, radius)
    gc.beginPath()
    gc.moveTo(points.head.x, points.head.y)
    points.tail.foreach(p => gc.lineTo(p.x, p.y))
    gc.lineTo(points.head.x, points.head.y)
    gc.fill()
    gc.stroke()
    gc.closePath()
  }

  // Calculate the points of a hexagon with the given (x, y) coordinates and radius
  def calculateHexagonPoints(x: Double, y: Double, radius: Double): Seq[Point2D] = {
    val points = for (i <- 0 to 5) yield {
      val angleDeg = -30+60 * i
      val angleRad = Math.PI / 180 * angleDeg
      val pointX = x + radius * Math.cos(angleRad)
      val pointY = y + radius * Math.sin(angleRad)
      Point2D(pointX, pointY)
    }
    points
  }

  case class Point2D(x: Double, y: Double)
}

object HexGame {
  var gameState: Hex.GameState = Hex.emptyGame
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[HexGameUI], args: _*)
  }
}