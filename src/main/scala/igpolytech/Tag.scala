package igpolytech
import scala.xml.Node

case class Tag(name: String, hash: String) {
  def save(saveTagToRepo: (String, String) => Unit) = {
    //TODO old
    // FilesIO.write(s"${tagsPath}$name", hash)
    saveTagToRepo(name, hash)
  }

  def getCommit(
      commitContent: (String) => Node,
      commitExists: (String) => Boolean
  ): Option[Commit] = {
    //TODO DELETE ME
    // val commitContent = (hash: String) =>
    //   FilesIO.loadXml(s"${commitsPath}${hash}")
    // val commitExists = (hash: String) =>
    //   FilesIO.fileExists(s"${commitsPath}$hash")

    Commit.getCommitOption(hash, commitContent, commitExists)
  }

}

object Tag {
  def exists(name: String, tagExists: (String) => Boolean): Boolean = {
    //TODO old
    // FilesIO.fileExists(s"${tagsPath}$name")
    tagExists(name)
  }

  def allTags(
      allTagsFiles: Array[String],
      tagContent: (String) => String
  ): Array[Tag] = {
    allTagsFiles
    // TODO old .map(tagName => Tag(tagName, FilesIO.getContent(s"${tagsPath}$tagName")))
      .map(tagName => Tag(tagName, tagContent(tagName)))
  }

  def fromName(
      name: String,
      tagExists: (String) => Boolean,
      tagContent: (String) => String
  ): Option[Tag] = {
    if (exists(name, tagExists))
      // TODO old Some(Tag(name, FilesIO.getContent(s"${tagsPath}$name")))
      Some(Tag(name, tagContent(name)))
    else None
  }
}
