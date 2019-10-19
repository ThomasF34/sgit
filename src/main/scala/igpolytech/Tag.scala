package igpolytech
import scala.xml.Node

case class Tag(name: String, hash: String) {
  def save(saveTagToRepo: (String, String) => Unit) =
    saveTagToRepo(name, hash)

  def getCommit(
      commitContent: (String) => Node,
      commitExists: (String) => Boolean
  ): Option[Commit] =
    Commit.getCommitOption(hash, commitContent, commitExists)

}

object Tag {
  def exists(name: String, tagExists: (String) => Boolean): Boolean =
    tagExists(name)

  def allTags(
      allTagsFiles: Array[String],
      tagContent: (String) => String
  ): Array[Tag] =
    allTagsFiles
      .map(tagName => Tag(tagName, tagContent(tagName)))

  def fromName(
      name: String,
      tagExists: (String) => Boolean,
      tagContent: (String) => String
  ): Option[Tag] =
    if (exists(name, tagExists))
      Some(Tag(name, tagContent(name)))
    else None
}
