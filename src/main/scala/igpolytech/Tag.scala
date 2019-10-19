package igpolytech

case class Tag(name: String, hash: String) {
  def save(tagsPath: String) = {
    FilesIO.write(s"${tagsPath}$name", hash)
  }

  def getCommit(commitsPath: String): Option[Commit] = {
    //TODO DELETE ME
    val commitContent = (hash: String) =>
      FilesIO.loadXml(s"${commitsPath}${hash}")
    val commitExists = (hash: String) =>
      FilesIO.fileExists(s"${commitsPath}$hash")

    Commit.getCommitOption(hash, commitContent, commitExists)
  }

}

object Tag {
  def exists(name: String, tagsPath: String): Boolean = {
    FilesIO.fileExists(s"${tagsPath}$name")
  }

  def allTags(tagsPath: String): Array[Tag] = {
    FilesIO
      .getAllFilesPath(tagsPath)
      .map(tagName => Tag(tagName, FilesIO.getContent(s"${tagsPath}$tagName")))
  }

  def fromName(name: String, tagsPath: String): Option[Tag] = {
    if (exists(name, tagsPath))
      Some(Tag(name, FilesIO.getContent(s"${tagsPath}$name")))
    else None
  }
}
