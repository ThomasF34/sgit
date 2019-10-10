package igpolytech

object ChangeType extends Enumeration {
  type ChangeType = Value
  val ADD, SUB = Value
}
import ChangeType._

case class Change(changeType: ChangeType, line: String) {
  override def toString(): String = {
    if (changeType == ADD) s"+ $line"
    else s"- $line"
  }
}
