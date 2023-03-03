import Cells.Cell

import scala.Console._
import scala.annotation.tailrec
import scala.io.StdIn.{readInt, readLine}

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


  def play(gameState: GameState, coordinate:(Int,Int), color:Cells.Cell):GameState=
  if(coordinate._1>=gameState.length || coordinate._2>=gameState(coordinate._1).length || color==Cells.Empty)
    throw new IllegalArgumentException
  else if(gameState(coordinate._1)(coordinate._2)!=Cells.Empty)
    throw new IllegalStateException
  else gameState.updated(coordinate._1,gameState(coordinate._1).updated(coordinate._2,color))

  def hasContiguousLine(gameState: GameState): Boolean ={
    @tailrec
    def isWinningLine(line: List[Cell]):Boolean =
      line match {
        case Cells.Empty::_ => false
        case cell1::cell2::Nil if cell1.equals(cell2) => true
        case cell1::cell2::rest if cell1.equals(cell2) => isWinningLine(cell2::rest)
        case _ =>false
      }

    @tailrec
    def hasWinningLine(gameState: GameState) :Boolean=
      gameState match {
        case Nil =>false
        case line::rest => isWinningLine(line) || hasWinningLine(rest)
      }
    val transposedGameState=gameState.transpose // To analyse columns: treat them as lines in a transposed gameState
    hasWinningLine(gameState) || hasWinningLine(transposedGameState)
  }

  @tailrec
  def askHumanPos(gameState: GameState):(Int,Int) = {
    println("Pf indique a linha onde quer jogar.")
    val line= readInt()
    println("Pf indique a coluna onde quer jogar.")
    val row=readInt()
    if(line>=0 && row>=0 && line<gameState.length && row<gameState(line).length && gameState(line)(row)==Cells.Empty)
      (line,row)
    else {
      println("Posiçõ inválida, pPF repita a introdução.2")
      askHumanPos(gameState)
    }
  }

  def askUserForMoveAcceptance() :Boolean= {
    println("Prima enter para continuar ou U/Undo para desfazer última jogada.")
    val answer = readLine()
    !answer.equals("U") && !answer.equals("Undo")
  }

  /* @return human won/ winning color */
  @tailrec
  def playLoop(gameState: GameState, humanPlaying: Boolean, color: Cell) :(Cell,Boolean)= {
    if(hasContiguousLine(gameState ))
      (Cells.opposite(color),!humanPlaying)
    else {
      val nextPosition=if(humanPlaying) askHumanPos(gameState)
      else computerMove(gameState)
      printInTUI(play(gameState,nextPosition,color))
      val accept=askUserForMoveAcceptance()
      if(accept)
        playLoop(play(gameState,nextPosition,color),!humanPlaying,Cells.opposite(color))
      else
        playLoop(gameState,humanPlaying,color)
    }
  }

  def playGame(humanPlaying: Boolean, humanColor: Cells.Cell): Unit ={
    val initialGame=emptyGame
    printInTUI(initialGame)
    val result=playLoop(emptyGame,humanPlaying,humanColor)
    val winner=if(result._2) "Human" else "Computer"
    println("Winning color:"+result._1 + ". "+winner+" has won.")
  }


    def printInTUI(gameState: GameState): Unit = {
      print(" ")
      var trailingSpaceCounter = 0
      for {
        i <- gameState.indices
      } {
        print(s"${BLUE}   *${RESET}")
      }
      println()
      for {
        line <- gameState
      } {
        print(" " * trailingSpaceCounter)
        print(s"${RED}*${RESET}")
        for {
          cell <- line
        } {
          print(" - ".concat(Cells.textRepresentation(cell)))
        }
        println(s"${RED}  *${RESET}")
        if (trailingSpaceCounter < 8) {
          print(" " * (trailingSpaceCounter + 5))
          for {
            i <- 0 to gameState.length - 2
          } {
            print("\\ / ")
          }
          println()
          trailingSpaceCounter += 2
        } else {
          print(" " * (trailingSpaceCounter + 1))
          for {
            i <- gameState.indices
          } {
            print(s"${BLUE}   *${RESET}")
          }
          println()
        }
      }
  }
  val emptyGame:GameState=List(List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty))
  val halfFilledGame:GameState=List(List(Cells.Empty,Cells.Red,Cells.Blue,Cells.Blue,Cells.Empty),List(Cells.Blue,Cells.Empty,Cells.Red,Cells.Empty,Cells.Blue),List(Cells.Blue,Cells.Blue,Cells.Red,Cells.Red,Cells.Empty),List(Cells.Red,Cells.Blue,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty))
  val finishedGame1:GameState=List(List(Cells.Empty,Cells.Red,Cells.Blue,Cells.Blue,Cells.Empty),List(Cells.Blue,Cells.Blue,Cells.Blue,Cells.Blue,Cells.Blue),List(Cells.Blue,Cells.Blue,Cells.Red,Cells.Red,Cells.Empty),List(Cells.Red,Cells.Blue,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty,Cells.Empty))
  val finishedGame2:GameState=List(List(Cells.Empty,Cells.Red,Cells.Blue,Cells.Blue,Cells.Empty),List(Cells.Blue,Cells.Red,Cells.Red,Cells.Empty,Cells.Blue),List(Cells.Blue,Cells.Red,Cells.Red,Cells.Red,Cells.Empty),List(Cells.Red,Cells.Red,Cells.Empty,Cells.Empty,Cells.Empty),List(Cells.Empty,Cells.Red,Cells.Empty,Cells.Empty,Cells.Empty))
  def main(args: Array[String]): Unit = {
    playGame(humanPlaying = true,Cells.Blue)
  }
}
