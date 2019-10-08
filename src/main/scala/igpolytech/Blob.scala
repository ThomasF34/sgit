package igpolytech

case class Blob(name: String) {
  override def toString(): String = name
  def hash: String = "Not yet implemented"
}
