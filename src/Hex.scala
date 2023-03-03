import Cells.Cell
import com.sun.org.apache.bcel.internal.classfile.LineNumber

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


  def removeInvalidPositions(gameState: GameState, value: List[(Int, Int)]): List[(Int, Int)] =
    value filter((position:(Int,Int))=> position._1>=0 && position._1<gameState.length && position._2>=0 && position._2<gameState(position._1).length)

  // Will return a list of connected positions. Must include all connected positions, as path may "snake" around the board
  def getConnectedPositions(gameState: GameState, lineNumber: Int, rowNumber: Int):List[(Int,Int)] = {
    removeInvalidPositions(gameState,List((lineNumber,rowNumber-1),(lineNumber,rowNumber+1),(lineNumber-1,rowNumber),
      (lineNumber-1,rowNumber+1),(lineNumber+1,rowNumber-1),(lineNumber+1,rowNumber)))
  }


  def hasContiguousLine(gameState: GameState): Boolean ={
    def isWinningPath(gameState: GameState,color:Cells.Cell,position:(Int,Int)):Boolean = {
      val currentPosition=gameState(position._1)(position._2)
      if(currentPosition!=color)
        false
      else{
        // check if end of board has been reached, in which casa the line exists
        if(position._2==gameState(position._1).length-1)
          true
        else {
          val connectedPositions: List[(Int, Int)] = getConnectedPositions(gameState, position._1, position._2)
          // Maybe use fold with isWinningPath applied to all connected positions?
          connectedPositions.foldRight(true)((iteratedPosition:(Int,Int),result:Boolean)=>result||isWinningPath(gameState,color,iteratedPosition))
        }
      }

    }

    def hasWinningLine(gameState: GameState) :Boolean= {
      @tailrec
      def hasWinningLineAux(gameState: GameState, lineNumber: Int): Boolean = {
        gameState match {
          case Nil =>false
          case (Cells.Empty::_)::_ => false // no winning line starting with empty!
          case line::rest  => isWinningPath(gameState, line.head,(lineNumber,0)) || hasWinningLineAux(rest,lineNumber+1)

        }
      }
      hasWinningLineAux(gameState, 0)
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
