package igpolytech

case class Blob(name: String, getContent: () => String) {
  def hash: String = GeneralHelper.generateHash(content)
  lazy val content: String = getContent();

  def save(writeBlobToRepo: (String, String) => Unit) = {
    //TODO old FilesIO.write(s"${dirPath}${hash}", content)
    writeBlobToRepo(hash, content)
  }

  def writeBlob(writeBlobToRepo: (String, String) => Unit) =
    writeBlobToRepo(name, content)
  //TODO old FilesIO.write(s"${dirPath}$name", content)

  def getDiffWithNew(
      projectDir: String,
      treeName: String,
      blobPath: String,
      blobExists: (String) => Boolean,
      blobContent: (String) => String,
      fileContent: (String) => String
  ): Option[Diff] = {
    if (blobExists(name)) {
      //TODO old if (FilesIO.fileExists(s"${projectDir}${treeName}${name}")) {
      Diff.fromContents(
        //TODO old FilesIO.getContent(s"${blobPath}${hash}"),
        blobContent(hash),
        fileContent(name),
        // TODO old FilesIO.getContent(s"${projectDir}${treeName}${name}"),
        s"${treeName}$name"
      )
    } else {
      Some(
        Diff.removedFile(
          blobContent(hash),
          // TODO old FilesIO.getContent(s"${blobPath}${hash}"),
          s"${treeName}$name"
        )
      )
    }
  }

  override def toString(): String = hash
}

object Blob {
  def getBlob(
      name: String,
      hash: String,
      blobContent: (String) => String
  ): Blob = {
    new Blob(
      name,
      // TODO old () => FilesIO.getContent(s"${dirPath}$hash")
      () => blobContent(hash)
    )
  }
}
