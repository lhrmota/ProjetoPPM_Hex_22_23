import scala.Console.{BLUE, RED, RESET, WHITE}

object Cells extends Enumeration {
  type Cell = Value
  val Red,  Blue, Empty=Value
  def textRepresentation(cell: Cell): String=
    cell match {
      case Red => s"${RED}X${RESET}"
      case Blue => s"${BLUE}O${RESET}"
      case Empty => s"${WHITE}.${RESET}"
    }
  def opposite(cell: Cell):Cell=
    if(cell==Blue)
      Red
    else if(cell==Red)
      Blue
    else throw new IllegalArgumentException
}


