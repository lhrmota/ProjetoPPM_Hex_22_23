import Cells.Cell
import HexGameUI.gameState
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.canvas.GraphicsContext
import javafx.scene.input.MouseEvent
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.stage.Stage

class HexGameUI extends Application {

  val hexRadius=50
  val humanColor: Cell = Cells.Blue
  var rand:RandomWithState=MyRandom(System.currentTimeMillis())

  def processClickOnHexagon(line: Int, row: Int): Unit = {
    gameState=Hex.play(gameState,(line,row),humanColor)
    val result=Hex.computerMove(gameState,Cells.opposite(humanColor),rand)
    rand=result._3
    gameState=Hex.play(gameState,(result._1,result._2),Cells.opposite(humanColor))
  }

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Hex Game")
    val root = new Pane
    val canvas = new Canvas(hexRadius*HexGameUI.gameState.length*2.8, hexRadius*HexGameUI.gameState.length*2)
    root.getChildren.add(canvas)
    val mainScene=new Scene(root)
    primaryStage.setScene(mainScene)
    primaryStage.show()

    val gc = canvas.getGraphicsContext2D
    drawHexagons(gc)

    canvas.setOnMouseClicked(event => {
        //println("Clicked on canvas"+event)
        val line=(event.getY-hexRadius)/hexRadius/1.5
        val rowRaw=(event.getX-hexRadius)/hexRadius/1.75
        val row=rowRaw-line.toInt*.5
        println("Clicked hex row:"+rowRaw+":"+row+" line:"+line)
        //  only accept clicks near the centre of the hexagon
        processClickOnHexagon(line.toInt,row.toInt)
        drawHexagons(gc)
        if(Hex.hasContiguousLine(gameState)) {
          // Remove event handler
          canvas.setOnMouseClicked(event => println("Game has ended!"))
        }
    })
  }

  def drawExternalLines(gc: GraphicsContext):Unit = {
    gc.setStroke(Color.BLUE)
    gc.setLineWidth(6)

    // Bottom
    gc.beginPath()
    gc.moveTo(gameState.length*hexRadius*.8, gameState.length*hexRadius*1.75)
    gc.lineTo(gameState.length*hexRadius*2.75,gameState.length*hexRadius*1.75)
    gc.fill()
    gc.stroke()
    gc.closePath()
    // Top
    gc.beginPath()
    gc.moveTo(0, hexRadius/4)
    gc.lineTo(gameState.length*hexRadius*2,hexRadius/4)
    gc.fill()
    gc.stroke()
    gc.closePath()
    // Left
    gc.setStroke(Color.RED)
    gc.beginPath()
    gc.moveTo( 0,hexRadius/4)
    gc.lineTo(gameState.length*hexRadius*.8,gameState.length*hexRadius*1.75)
    gc.fill()
    gc.stroke()
    gc.closePath()
    // Left
    gc.beginPath()
    gc.moveTo( gameState.length*hexRadius*2,hexRadius/4)
    gc.lineTo(gameState.length*hexRadius*2+gameState.length*hexRadius*.8,gameState.length*hexRadius*1.75)
    gc.fill()
    gc.stroke()
    gc.closePath()
  }
  def drawHexagons(gc: GraphicsContext):Unit = {
  // top and bottom lines
    drawExternalLines(gc)
    // Draw the hexagons
    for( line <- HexGameUI.gameState.indices)
      for( column<- HexGameUI.gameState.indices) {
        val hexColor= gameState(line)(column) match {
          case Cells.Red => Color.RED
          case Cells.Blue => Color.BLUE
          case Cells.Empty => Color.WHITE
        }
        drawHexagon(gc, (column+1+line*.5)*hexRadius*1.75, (line+1)*hexRadius*1.5, hexRadius, hexColor)
      }
    //drawHexagon(gc, 135, 50, hexRadius, Color.BLUE)
    //drawHexagon(gc, 93, 125, hexRadius, Color.GREEN)
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

object HexGameUI {
  var gameState: Hex.GameState = Hex.emptyGame
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[HexGameUI], args: _*)
  }
}