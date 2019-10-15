package igpolytech
import java.io.File

case class Blob(name: String, getContent: () => String) {
  def hash: String = FilesIO.generateHash(content)
  lazy val content: String = getContent();

  def save(dirPath: String) = {
    FilesIO.write(s"${dirPath}${hash}", content)
  }

  def writeBlob(dirPath: String) = FilesIO.write(s"${dirPath}$name", content)

  def getDiffWithNew(
      projectDir: String,
      treeName: String,
      blobPath: String
  ): Option[Diff] = {
    if (FilesIO.fileExists(s"${projectDir}${treeName}${name}")) {
      Diff.fromContents(
        FilesIO.getContent(s"${blobPath}${hash}"),
        FilesIO.getContent(s"${projectDir}${treeName}${name}"),
        s"${treeName}$name"
      )
    } else {
      Some(
        Diff.removedFile(
          FilesIO.getContent(s"${blobPath}${hash}"),
          s"${treeName}$name"
        )
      )
    }
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
