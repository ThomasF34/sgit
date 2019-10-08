package igpolytech
import scala.io.Source
import java.io.File

case class Blob(name: String, getContent: () => String) {
  def hash: String = FilesIO.generateHash(content)
  lazy val content: String = getContent();

  def save(dirPath: String) = {
    FilesIO.write(s"${dirPath}${hash}", content)
    println(s"Had saved $content in ${dirPath}${hash}")
  }

  override def toString(): String = hash
}

object Blob {
  def getBlob(name: String, dirPath: String, hash: String): Blob = {
    println(s"$name : dirPath for future Get Content $dirPath for hash $hash")
    new Blob(
      name,
      () => FilesIO.getContent(s"${dirPath}$hash")
    )
  }
}
