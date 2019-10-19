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

  def save(
      saveCommitToRepo: (String, Node) => Unit
  ): Unit = {
    saveCommitToRepo(hash, toXml())
  }

  def getDetails(
      withStats: Boolean,
      withDiff: Boolean,
      blobContent: (String) => String,
      treeContent: (String) => Node,
      commitContent: (String) => Node
  ): String = {
    val base =
      s"commit $hash\nAuthor: $author\nDate: ${timestamp.toString()}\n$text\n"

    (withStats, withDiff) match {
      case (true, true) =>
        s"${base}Stats:\n${getStats(blobContent, treeContent, commitContent)
          .mkString("\n")}\n\nDiff:\n${getDetailedDiff(
          blobContent,
          treeContent,
          commitContent
        ).mkString("\n")}\n===="
      case (true, false) =>
        s"${base}Stats:\n${getStats(blobContent, treeContent, commitContent)
          .mkString("\n")}\n===="
      case (false, true) =>
        s"${base}Diff:\n${getDetailedDiff(
          blobContent,
          treeContent,
          commitContent
        ).mkString("\n")}\n===="
      case (false, false) => s"$base\n===="
    }
  }

  private def getDiffWithParent(
      blobContent: (String) => String,
      treeContent: (String) => Node,
      commitContent: (String) => Node
  ): Array[Diff] = {
    if (parentHash != "") {
      val tree = Tree.getTree(
        treeHash,
        blobContent,
        treeContent
      )
      val parentTree = Tree.getTree(
        Commit.getCommit(parentHash, commitContent).treeHash,
        blobContent,
        treeContent
      )
      Diff.fromTrees(parentTree, tree)
    } else {
      Array()
    }
  }

  private def getDetailedDiff(
      blobContent: (String) => String,
      treeContent: (String) => Node,
      commitContent: (String) => Node
  ): Array[String] = {
    val diff =
      getDiffWithParent(
        blobContent,
        treeContent,
        commitContent
      ).map(_.getDetails)
    if (diff.isEmpty) Array("-- No diff --")
    else diff
  }
  private def getStats(
      blobContent: (String) => String,
      treeContent: (String) => Node,
      commitContent: (String) => Node
  ): Array[String] = {
    val stats =
      getDiffWithParent(
        blobContent,
        treeContent,
        commitContent
      ).map(_.getStats)
    if (stats.isEmpty) Array("-- No diff --")
    else stats
  }

  def getParentCommit(
      commitContent: (String) => Node,
      commitExists: (String) => Boolean
  ): Option[Commit] = {
    if (parentHash != "")
      Commit.getCommitOption(
        parentHash,
        commitContent,
        commitExists
      )
    else None
  }

  def hasCommitAsAncestor(
      commit: Commit,
      commitContent: (String) => Node,
      commitExists: (String) => Boolean
  ): Boolean = {
    getParentCommit(commitContent, commitExists) match {
      case Some(parentCommit) =>
        if (parentCommit.equals(commit)) {
          true
        } else
          parentCommit.hasCommitAsAncestor(
            commit,
            commitContent,
            commitExists
          )
      case None => false
    }
  }
}

object Commit {
  def getCommitOption(
      hash: String,
      commitContent: (String) => Node,
      commitExists: (String) => Boolean
  ): Option[Commit] = {
    if (Commit.exists(hash, commitExists))
      Some(getCommit(hash, commitContent))
    else None
  }

  def getCommit(
      hash: String,
      commitContent: (String) => Node
  ): Commit = {
    val xmlContent = commitContent(hash)
    val text = (xmlContent \ "text").text
    val parentHash = (xmlContent \ "parent").text
    val author = (xmlContent \ "author").text
    val treeHash = (xmlContent \ "tree").text
    val timestamp = Instant.parse((xmlContent \ "timestamp").text)
    new Commit(treeHash, parentHash, text, author, timestamp)
  }

  def exists(
      hash: String,
      commitExists: (String) => Boolean
  ): Boolean =
    commitExists(hash)

  def getAncestorCommit(
      firstCommit: Commit,
      secondCommit: Commit,
      commitContent: (String) => Node,
      commitExists: (String) => Boolean
  ): Option[Commit] = {
    secondCommit.getParentCommit(commitContent, commitExists) match {
      case Some(parentCommit) =>
        if (firstCommit.hasCommitAsAncestor(
              parentCommit,
              commitContent,
              commitExists
            ))
          Some(parentCommit)
        else
          getAncestorCommit(
            firstCommit,
            parentCommit,
            commitContent,
            commitExists
          )
      case None => None
    }
  }
}
