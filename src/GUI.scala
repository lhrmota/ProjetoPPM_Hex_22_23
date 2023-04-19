import javafx.fxml.FXML
import javafx.scene.control.{Label, TextField}
import javafx.scene.layout.GridPane

class GUI {
  @FXML
  private var gridPane: GridPane = _
  //  @FXML
//  private var passwordField: PasswordField = _

  def onUndoButtonClicked(): Unit = {
    println("onUndoButtonClicked")
  }


}
