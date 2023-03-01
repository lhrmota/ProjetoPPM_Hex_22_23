import scala.annotation.tailrec

object Hex {
  type GameState=List[List[Cells.Cell]]

  // @return is (line,row)
  @tailrec
  def computerMove(gameState: GameState):(Int,Int)= {
   val line:Int=(gameState.length*Math.random()).toInt
   val row:Int=(gameState.length*Math.random()).toInt
   if(gameState(line)(row)==Cells.Empty)
     (line,row)
   else Hex.computerMove(gameState)
 }
def play(gameState: GameState, coordinate:(Int,Int),color:Cells.Cell):GameState=
  if(coordinate._1>=gameState.length || coordinate._2>=gameState(coordinate._1).length || color==Cells.Empty)
    throw new IllegalArgumentException
  else if(gameState(coordinate._1)(coordinate._2)!=Cells.Empty)
    throw new IllegalStateException
  else gameState.updated(coordinate._1,gameState(coordinate._1).updated(coordinate._2,color))


  val emptyGame:GameState=List(List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty))
  val halfFilledGame:GameState=List(List(Cells.Empty,Cells.Red,Cells.Blue,Cells.Blue,Cells.Empty),List(Cells.Blue,Cells.Empty,Cells.Red,Cells.Empty,Cells.Blue),List(Cells.Blue,Cells.Blue,Cells.Red,Cells.Red,Cells.Empty),List(Cells.Red,Cells.Blue,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty))
}
