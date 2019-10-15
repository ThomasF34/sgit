package igpolytech
import scala.collection.immutable.Stream.Cons

object ChangeType extends Enumeration {
  type ChangeType = Value
  val ADD, SUB = Value
}
import ChangeType._

case class Change(changeType: ChangeType, line: String, lineNumber: Int) {
  override def toString(): String = {
    if (changeType == ADD)
      s"${lineNumber}: ${Console.GREEN}+ ${line}${Console.RESET}"
    else s"${lineNumber}: ${Console.RED}- ${line}${Console.RESET}"
  }
}
