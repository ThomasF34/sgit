package igpolytech

import scala.xml.Node
import java.time.Instant

case class Commit(
    treeHash: String,
    parentHash: String,
    text: String,
    author: String,
    timestamp: Instant = Instant.now()
) {
  def title: String = {
    text.split("\n")(0) match {
      case str: String if str.length >= 50 => str.slice(0, 51)
      case str                             => str
    }
  }

  def hash: String = {
    FilesIO.generateHash(
      s"${treeHash}${parentHash}${text}${author}${timestamp}"
    )
  }

  def toXml(): Node = {
    <Commit>
      <parent>{parentHash}</parent>
      <author>{author}</author>
      <text>{text}</text>
      <tree>{treeHash}</tree>
      <timestamp>{timestamp}</timestamp>
    </Commit>
  }

  def save(commitsDirPath: String): Unit = {
    FilesIO.saveXml(this.toXml(), s"${commitsDirPath}${hash}")
  }
}

object Commit {
  def getCommitOption(hash: String, commitsDirPath: String): Option[Commit] = {
    if (Commit.exists(hash, commitsDirPath))
      Some(getCommit(hash, commitsDirPath))
    else None
  }
  def getCommit(hash: String, commitsDirPath: String): Commit = {
    val xml = FilesIO.loadXml(s"${commitsDirPath}${hash}")
    getCommit(xml)
  }

  def exists(hash: String, commitsDirPath: String): Boolean =
    FilesIO.fileExists(s"${commitsDirPath}$hash")

  def getCommit(
      xmlContent: Node
  ): Commit = {
    val text = (xmlContent \ "text").text
    val parentHash = (xmlContent \ "parent").text
    val author = (xmlContent \ "author").text
    val treeHash = (xmlContent \ "tree").text
    val timestamp = Instant.parse((xmlContent \ "timestamp").text)
    new Commit(treeHash, parentHash, text, author, timestamp)

  }
}
