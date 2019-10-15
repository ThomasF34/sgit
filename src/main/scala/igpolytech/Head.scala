package igpolytech
import java.io.File
import scala.xml.Node

case class Head(mode: String, commit: Commit) {
  def save(headPath: String) = FilesIO.saveXml(this.toXml(), headPath)

  def update(newCommit: Commit): Head = {
    Head(mode, newCommit)
  }

  def toXml(): Node = <Head mode={mode}>{commit.hash}</Head>
}

object Head {
  def exists(branchesPath: String, branchName: String): Boolean =
    FilesIO.fileExists(s"${branchesPath}$branchName")

  def fromCommitHash(commitHash: String, commitsPath: String): Head =
    Head("detached", Commit.getCommit(commitHash, commitsPath))

  def fromBranchName(
      branchName: String,
      branchesPath: String,
      commitsPath: String
  ): Option[Head] = {
    if (exists(branchesPath, branchName)) {
      val commitHashOption = FilesIO.getHash(s"${branchesPath}$branchName")
      commitHashOption
        .map(commitHash => Commit.getCommit(commitHash, commitsPath))
        .map(commit => Head("branch", commit))
    } else None
  }

  def fromHeadFile(headPath: String, commitsPath: String): Option[Head] = {
    if (FilesIO.emptyFile(headPath)) None
    else {
      val xml = FilesIO.loadXml(headPath)
      val mode = (xml \@ "mode")
      val commitRef = (xml).text
      Some(Head(mode, Commit.getCommit(commitRef, commitsPath)))
    }
  }
}
