import scala.annotation.tailrec

object Hex {
  type GameState=List[List[Cells.Cell]]

  // output is (line,row)
  @tailrec
  def computerMove(gameState: GameState):(Int,Int)= {
   val line:Int=(gameState.length*Math.random()).toInt
   val row:Int=(gameState.length*Math.random()).toInt
   if(gameState(line)(row)==Cells.Empty)
     (line,row)
   else Hex.computerMove(gameState)
 }
//def play(gameState: GameState, coordinate:(Int,Int),color:Cells.Cell):GameState=


  val emptyGame:GameState=List(List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty))
  val halfFilledGame:GameState=List(List(Cells.Empty,Cells.Red,Cells.Blue,Cells.Blue,Cells.Empty),List(Cells.Blue,Cells.Empty,Cells.Red,Cells.Empty,Cells.Blue),List(Cells.Blue,Cells.Blue,Cells.Red,Cells.Red,Cells.Empty),List(Cells.Red,Cells.Blue,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty))
}
