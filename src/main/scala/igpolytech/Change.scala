package igpolytech
import scala.collection.immutable.Stream.Cons

object ChangeType extends Enumeration {
  type ChangeType = Value
  val ADD, SUB = Value
}
import ChangeType._

case class Change(changeType: ChangeType, line: String) {
  override def toString(): String = {
    if (changeType == ADD) s"${Console.GREEN}+ ${line}${Console.RESET}"
    else s"${Console.RED}- ${line}${Console.RESET}"
  }
}
