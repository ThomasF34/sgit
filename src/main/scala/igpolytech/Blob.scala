package igpolytech

case class Blob(name: String, getContent: () => String) {
  def hash: String = GeneralHelper.generateHash(content)
  lazy val content: String = getContent();

  def save(writeBlobToRepo: (String, String) => Unit) =
    writeBlobToRepo(hash, content)

  def writeBlob(writeBlobToRepo: (String, String) => Unit) =
    writeBlobToRepo(name, content)

  def getDiffWithNew(
      treeName: String,
      blobExists: (String) => Boolean,
      blobContent: (String) => String,
      fileContent: (String) => String
  ): Option[Diff] =
    if (blobExists(name)) {
      Diff.fromContents(
        blobContent(hash),
        fileContent(name),
        s"${treeName}$name"
      )
    } else {
      Some(
        Diff.removedFile(
          blobContent(hash),
          s"${treeName}$name"
        )
      )
    }

  override def toString(): String = hash
}

object Blob {
  def getBlob(
      name: String,
      hash: String,
      blobContent: (String) => String
  ): Blob =
    new Blob(
      name,
      () => blobContent(hash)
    )
}
