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
    GeneralHelper.generateHash(
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

  def getDetails(
      withStats: Boolean,
      withDiff: Boolean,
      pathTreeDir: String,
      pathBlobDir: String,
      commitsDir: String
  ): String = {
    val base =
      s"commit $hash\nAuthor: $author\nDate: ${timestamp.toString()}\n$text\n"

    (withStats, withDiff) match {
      case (true, true) =>
        s"${base}Stats:\n${getStats(pathTreeDir, pathBlobDir, commitsDir)
          .mkString("\n")}\n\nDiff:\n${getDetailedDiff(
          pathTreeDir,
          pathBlobDir,
          commitsDir
        ).mkString("\n")}\n===="
      case (true, false) =>
        s"${base}Stats:\n${getStats(pathTreeDir, pathBlobDir, commitsDir).mkString("\n")}\n===="
      case (false, true) =>
        s"${base}Diff:\n${getDetailedDiff(
          pathTreeDir,
          pathBlobDir,
          commitsDir
        ).mkString("\n")}\n===="
      case (false, false) => s"$base\n===="
    }
  }

  private def getDiffWithParent(
      pathTreeDir: String,
      pathBlobDir: String,
      commitsDir: String
  ): Array[Diff] = {
    if (parentHash != "") {
      val tree = Tree.getTree(pathTreeDir, pathBlobDir, treeHash)
      val parentTree = Tree.getTree(
        pathTreeDir,
        pathBlobDir,
        Commit.getCommit(parentHash, commitsDir).treeHash
      )
      Diff.fromTrees(parentTree, tree)
    } else {
      Array()
    }
  }

  private def getDetailedDiff(
      pathTreeDir: String,
      pathBlobDir: String,
      commitsDir: String
  ): Array[String] = {
    val diff =
      getDiffWithParent(pathTreeDir, pathBlobDir, commitsDir).map(_.getDetails)
    if (diff.isEmpty) Array("-- No diff --")
    else diff
  }
  private def getStats(
      pathTreeDir: String,
      pathBlobDir: String,
      commitsDir: String
  ): Array[String] = {
    val stats =
      getDiffWithParent(pathTreeDir, pathBlobDir, commitsDir).map(_.getStats)
    if (stats.isEmpty) Array("-- No diff --")
    else stats
  }

  def getParentCommit(commitsPath: String): Option[Commit] = {
    if (parentHash != "") Commit.getCommitOption(parentHash, commitsPath)
    else None
  }

  def hasCommitAsAncestor(commit: Commit, commitsPath: String): Boolean = {
    getParentCommit(commitsPath) match {
      case Some(parentCommit) =>
        if (parentCommit.equals(commit)) {
          true
        } else parentCommit.hasCommitAsAncestor(commit, commitsPath)
      case None => false
    }
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

  def getAncestorCommit(
      firstCommit: Commit,
      secondCommit: Commit,
      commitsPath: String
  ): Option[Commit] = {
    secondCommit.getParentCommit(commitsPath) match {
      case Some(parentCommit) =>
        if (firstCommit.hasCommitAsAncestor(parentCommit, commitsPath))
          Some(parentCommit)
        else
          getAncestorCommit(
            firstCommit,
            parentCommit,
            commitsPath
          )
      case None => None
    }
  }
}
