import Cells.Cell

import scala.Console._
import scala.annotation.tailrec
import scala.io.StdIn.{readInt, readLine}

object Hex {
  type GameState = List[List[Cells.Cell]]

  /* @return human won/ winning color */
  @tailrec
  def playLoop(gameState: GameState, humanPlaying: Boolean, color: Cell, rand: RandomWithState): (Cell, Boolean, RandomWithState) = {
    if (hasContiguousLine(gameState))
      (Cells.opposite(color), !humanPlaying, rand)
    else {
      val nextPosition = if (humanPlaying) {
        val pos = askHumanPos(gameState)
        (pos._1, pos._2, rand)
      }
      else computerMove(gameState, color, rand)
      // Debug
      println("Extremes: red->" + extremePositionsForColor(play(gameState, (nextPosition._1, nextPosition._2), color), Cells.Red) +
        "blue->" + extremePositionsForColor(play(gameState, (nextPosition._1, nextPosition._2), color), Cells.Blue))
      printInTUI(play(gameState, (nextPosition._1, nextPosition._2), color))
      val accept = askUserForMoveAcceptance()
      if (accept)
        playLoop(play(gameState, (nextPosition._1, nextPosition._2), color), !humanPlaying, Cells.opposite(color), rand)
      else
        playLoop(gameState, humanPlaying, color, rand)
    }
  }



  // @return is (line,row,RandomWithState)
  @tailrec
  def computerMove(gameState: GameState, color: Cells.Cell, rand: RandomWithState): (Int, Int, RandomWithState) = {
    val extremePositions=extremePositionsForColor(gameState,color)
    if(extremePositions._1.isEmpty)// first move: get a position near the middle...
        // will be (3,3), or, if this is occupied, (3,2)
      if(gameState(3)(3).equals(Cells.opposite(color)))
        (3,2,rand)
      else
        (3,3,rand)
    else
      if(extremePositions._1.head._2==0) // left is already at column 0... Nothing to do here... Will try to play at the right
        if(extremePositions._2.head._2==gameState.length-1) { // right is already at column length-1... Nothing to do here... Will try to play randomt
          val line: (Int, RandomWithState) = rand.nextInt(gameState.length)
          val row: (Int, RandomWithState) = line._2.nextInt(gameState.length)
          if (gameState(line._1)(row._1) == Cells.Empty)
            (line._1, row._1, row._2)
          else Hex.computerMove(gameState, color, row._2)
        }
        else { // play at right... First get all vacant adjacent cells to the right, and choose one randomly
          val possibleCells=extremePositions._2.foldRight(List[(Int,Int)]())((position:(Int,Int),runningList:List[(Int,Int)])=>connectedFreeCellsToTheRight(gameState,position):::runningList)
          //  choose random position
          val nextRand = rand.nextInt(possibleCells.length)
          val position = possibleCells(nextRand._1)
          // DEBUG
          println("Possible cells to the right:"+possibleCells+" rand index:"+nextRand._1+" chosen position:"+possibleCells(nextRand._1))
          (possibleCells(nextRand._1)._1,possibleCells(nextRand._1)._2,nextRand._2)
        }
        else {// play at left... First get all vacant adjacent cells to the left, and choose one randomly
          val possibleCells = extremePositions._1.foldRight(
            List[(Int, Int)]())((position: (Int, Int), runningList: List[(Int, Int)]) => Hex.connectedFreeCellsToTheLeft(gameState, position) ::: runningList)
        // choose random position
        val nextRand = rand.nextInt(possibleCells.length)
        val position = possibleCells(nextRand._1)
        // DEBUG
        println("Possible cells to the left:" + possibleCells+" rand index:"+nextRand._1+" chosen position:"+possibleCells(nextRand._1))
        (possibleCells(nextRand._1)._1,possibleCells(nextRand._1)._2,nextRand._2)
      }
    // Old implementation, purely random
    /*val line: (Int, RandomWithState) = rand.nextInt(gameState.length)
    val row: (Int, RandomWithState) = line._2.nextInt(gameState.length)
    if (gameState(line._1)(row._1) == Cells.Empty)
      (line._1, row._1, row._2)
    else Hex.computerMove(gameState, row._2)*/
  }

  def play(gameState: GameState, coordinate: (Int, Int), color: Cells.Cell): GameState =
    if (coordinate._1 >= gameState.length || coordinate._2 >= gameState(coordinate._1).length || color == Cells.Empty)
      throw new IllegalArgumentException
    else if (gameState(coordinate._1)(coordinate._2) != Cells.Empty)
      throw new IllegalStateException
    else gameState.updated(coordinate._1, gameState(coordinate._1).updated(coordinate._2, color))

  def removeInvalidPositions(gameState: GameState, value: List[(Int, Int)]): List[(Int, Int)] =
    value filter ((position: (Int, Int)) => position._1 >= 0 && position._1 < gameState.length && position._2 >= 0 && position._2 < gameState(position._1).length)

  // Will return a list of connected positions. Must include all connected positions, as path may "snake" around the board
  def getConnectedPositions(gameState: GameState, position: (Int, Int)): List[(Int, Int)] = {
    removeInvalidPositions(gameState, List((position._1, position._2 - 1), (position._1, position._2 + 1), (position._1 - 1, position._2),
      (position._1 - 1, position._2 + 1), (position._1 + 1, position._2 - 1), (position._1 + 1, position._2)))
  }

  def removeOccupedPositions(gameState: GameState, value: List[(Int, Int)]): List[(Int, Int)] =
    value match {
      case ::(position, next) =>
        if(gameState(position._1)(position._2).equals(Cells.Empty))
          position::removeOccupedPositions(gameState,next)
        else removeOccupedPositions(gameState,next)
      case Nil => Nil
    }

  def connectedFreeCellsToTheRight(gameState: GameState, position: (Int, Int)): List[(Int, Int)] = {
    // TODO seems to be wrong! Check!
    removeOccupedPositions(gameState,removeInvalidPositions(gameState, List((position._1-1, position._2 +1),
      (position._1, position._2 + 1),(position._1 + 1, position._2 ))))
  }

  def connectedFreeCellsToTheLeft(gameState: GameState, position: (Int, Int)): List[(Int, Int)] =
    removeOccupedPositions(gameState, removeInvalidPositions(gameState, List((position._1, position._2 - 1),
      (position._1 - 1, position._2), (position._1 - 1, position._2+1))))
  def hasContiguousLine(gameState: GameState): Boolean = {
    def isWinningPath(gameState: GameState, color: Cells.Cell, position: (Int, Int), visitedPositions: List[(Int, Int)]): Boolean = {
      val currentCell = gameState(position._1)(position._2)
      if (currentCell != color)
        false
      else {
        // check if end of board has been reached, in which casa the line exists
        if (position._2 == gameState(position._1).length - 1)
          true
        else {
          val connectedPositions: List[(Int, Int)] = getConnectedPositions(gameState, position)
          val notVisitedConnectedPositions = connectedPositions.diff(visitedPositions)
          // Maybe use fold with isWinningPath applied to all connected positions?
          notVisitedConnectedPositions.foldRight(false)((iteratedPosition: (Int, Int), result: Boolean) =>
            result || isWinningPath(gameState, color, iteratedPosition, position :: visitedPositions))
        }
      }
    }

    // simply iterates all existing initial positions and checks, for each, if it starts a winning path
    def hasWinningLine(gameState: GameState): Boolean = {
      def hasWinningLineAux(gameState: GameState, lineNumber: Int): Boolean =
        gameState(lineNumber) match {
          case Nil => false
          case Cells.Empty :: _ => false // no winning line starting with empty!
          case head :: rest => isWinningPath(gameState, head, (lineNumber, 0), Nil)
        }

      val existingLines = gameState.indices
      existingLines.foldLeft(false)((value: Boolean, lineNumber: Int) => value || hasWinningLineAux(gameState, lineNumber))
    }

    val transposedGameState = gameState.transpose // To analyse columns: treat them as lines in a transposed gameState
    hasWinningLine(gameState) || hasWinningLine(transposedGameState)
  }

  @tailrec
  def askHumanPos(gameState: GameState): (Int, Int) = {
    println("Pf indique a linha onde quer jogar.")
    val line = readInt()
    println("Pf indique a coluna onde quer jogar.")
    val row = readInt()
    if (line >= 0 && row >= 0 && line < gameState.length && row < gameState(line).length && gameState(line)(row) == Cells.Empty)
      (line, row)
    else {
      println("Posição inválida, PF repita a introdução.")
      askHumanPos(gameState)
    }
  }
  @tailrec
  def askUserForMoveAcceptance(): Boolean = {
    println("Prima 'C' para continuar ou u/U/Undo para desfazer última jogada.")
    val answer = readLine()
    if(!answer.equals("U") && !answer.equals("Undo") && !answer.equals("u") && !answer.equals("C") && !answer.equals("c"))
      askUserForMoveAcceptance()
    else
      !answer.equals("U") && !answer.equals("Undo") && !answer.equals("u")
  }


  def playGame(humanPlaying: Boolean, humanColor: Cells.Cell, dimension: Int, rand: RandomWithState): Unit = {
    val initialGame = createEmptyGame(dimension)
    printInTUI(initialGame)
    val result = playLoop(initialGame, humanPlaying, humanColor, rand)
    val winner = if (result._2) "Human" else "Computer"
    println("Winning color:" + result._1 + ". " + winner + " has won.")
  }

  private def createEmptyGame(dimension: Int): GameState =
    List.fill(dimension)(List.fill(dimension)(Cells.Empty))

  // Change all line indexes to the complement- E.g 0-> length-1, length-2->1
  def invertLineNumbers(cells: List[(Int, Int)], gridSize: Int): List[(Int, Int)] = cells match {
    case Nil => Nil
    case head :: next => (gridSize - 1 - head._1, head._2) :: invertLineNumbers(next, gridSize)
  }

  def invertCoordinates(uppermostCells: List[(Int, Int)]): List[(Int, Int)] =
    uppermostCells map(in=>(in._2,in._1))

  // Outputs a pair with two lists: the first contains the positions with the given color more to the left/up, the second
  // the positions more to the right/down
  private def extremePositionsForColor(gameState: GameState, color: Cells.Cell): (List[(Int, Int)], List[(Int, Int)]) = {
    @tailrec
    def iterateLines(gameState: GameState, color: Cell, existingLines: List[Int]): List[(Int, Int)] =
    // check line by line and stop when color is found
      existingLines match {
        case Nil => Nil
        case lineNumber :: rest => val cellsOfColor = cellsOfColorAux(gameState(lineNumber), lineNumber, 0, color)
          if (cellsOfColor.isEmpty) iterateLines(gameState, color, rest)
          else cellsOfColor
      }
    def cellsOfColorAux(line: List[Cell], lineNumber: Int, rowNumber: Int, color: Cells.Cell): List[(Int, Int)] =
    // recurses over the line and returns all positions where color is found
      line match {
        case Nil => Nil
        case first :: rest => if (first.equals(color)) (lineNumber, rowNumber) :: cellsOfColorAux(rest, lineNumber, rowNumber + 1, color)
        else cellsOfColorAux(rest, lineNumber, rowNumber + 1, color)
      }

    // relevant color in columns is red... will check line by line and get all positions in that line
    // for blue, will transpose and do the same if red
    val existingLines = gameState.indices.toList
    if (color.equals(Cells.Blue)) {
      val uppermostCells = iterateLines(gameState, color, existingLines)
      val lowerCells = iterateLines(gameState.reverse, color, existingLines)
      (uppermostCells, invertLineNumbers(lowerCells, gameState.length))
    }
    else{
      val uppermostCells = iterateLines(gameState.transpose, color, existingLines)
      val lowerCells = iterateLines(gameState.transpose.reverse, color, existingLines)
      (invertCoordinates(uppermostCells), invertCoordinates(invertLineNumbers(lowerCells, gameState.length)))
    }
  }

  //  def reverseLines(gameState: GameState): GameState =
  //    gameState match {
  //      case Nil => Nil
  //      case head :: next => head.reverse :: reverseLines(next)
  //    }


  //  val transposedGameState=gameState.transpose // To analyse columns: treat them as lines in a transposed gameState
  //  hasWinningLine(gameState) || hasWinningLine(transposedGameState)
  // ---------------------------
  // Output
  // ---------------------------
  def printInTUI(gameState: GameState): Unit = {
    print(" ")
    var trailingSpaceCounter = 0
    for {
      i <- gameState.indices
    } {
      print(s"$BLUE   *$RESET")
    }
    println()
    for {
      line <- gameState
    } {
      print(" " * trailingSpaceCounter)
      print(s"$RED*$RESET")
      for {
        cell <- line
      } {
        print(" - ".concat(Cells.textRepresentation(cell)))
      }
      println(s"$RED  *$RESET")
      if (trailingSpaceCounter < (gameState.length - 1) * 2) {
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
          print(s"$BLUE   *$RESET")
        }
        println()
      }
    }
  }

  // -----------------------
  // Initial setups for testing
  // -----------------------
  val emptyGame: GameState = List(List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty), List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty), List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty), List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty), List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty))
  val halfFilledGame: GameState = List(List(Cells.Empty, Cells.Red, Cells.Blue, Cells.Blue, Cells.Empty),
    List(Cells.Blue, Cells.Empty, Cells.Red, Cells.Empty, Cells.Blue),
    List(Cells.Blue, Cells.Blue, Cells.Red, Cells.Red, Cells.Empty),
    List(Cells.Red, Cells.Blue, Cells.Empty, Cells.Empty, Cells.Empty),
    List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty))
  val finishedGame2Line: GameState = List(List(Cells.Empty, Cells.Red, Cells.Blue, Cells.Blue, Cells.Empty), List(Cells.Blue, Cells.Blue, Cells.Blue, Cells.Blue, Cells.Blue), List(Cells.Blue, Cells.Blue, Cells.Red, Cells.Red, Cells.Empty), List(Cells.Red, Cells.Blue, Cells.Empty, Cells.Empty, Cells.Empty), List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty))
  val finishedGame2Row: GameState = List(List(Cells.Empty, Cells.Red, Cells.Blue, Cells.Blue, Cells.Empty), List(Cells.Blue, Cells.Red, Cells.Red, Cells.Empty, Cells.Blue), List(Cells.Blue, Cells.Red, Cells.Red, Cells.Red, Cells.Empty), List(Cells.Red, Cells.Red, Cells.Empty, Cells.Empty, Cells.Empty), List(Cells.Empty, Cells.Red, Cells.Empty, Cells.Empty, Cells.Empty))
  val finishedGameSnakeLine: GameState = List(List(Cells.Empty, Cells.Red, Cells.Blue, Cells.Red, Cells.Empty),
    List(Cells.Blue, Cells.Blue, Cells.Empty, Cells.Blue, Cells.Blue),
    List(Cells.Empty, Cells.Empty, Cells.Red, Cells.Red, Cells.Empty),
    List(Cells.Red, Cells.Blue, Cells.Empty, Cells.Empty, Cells.Empty),
    List(Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty, Cells.Empty))


  def main(args: Array[String]): Unit = {
    playGame(humanPlaying = true, Cells.Blue, 6, MyRandom(System.currentTimeMillis()))
  }
}
