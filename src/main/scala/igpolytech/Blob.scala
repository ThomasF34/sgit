package igpolytech
import java.io.File

case class Blob(name: String, getContent: () => String) {
  def hash: String = FilesIO.generateHash(content)
  lazy val content: String = getContent();

  def save(dirPath: String) = {
    FilesIO.write(s"${dirPath}${hash}", content)
  }

  override def toString(): String = hash
}

object Blob {
  def getBlob(name: String, dirPath: String, hash: String): Blob = {
    new Blob(
      name,
      () => FilesIO.getContent(s"${dirPath}$hash")
    )
  }
}
