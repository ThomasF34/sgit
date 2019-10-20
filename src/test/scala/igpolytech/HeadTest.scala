package igpolytech

import scala.collection.mutable
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.xml.Node

class HeadTest extends FunSpec with Matchers {
  describe("Head saving") {
    it("should save Head to Repo") {
      val head = Head("branch", "master")

      val fakeHeadFileContent =
        mutable.Map[String, Node](("HEAD", head.toXml()))
      val mockSaveHead =
        (content: Node) => fakeHeadFileContent("HEAD") = content

      head.save(mockSaveHead)

      fakeHeadFileContent("HEAD") shouldBe head.toXml()
    }

    it("should update Head content when branch mode") {
      val head = Head("branch", "master")
      val master = Branch("master", "commit1")
      val fakeBranchRepoDir = mutable.Map[String, String](
        (master.name, master.commitHash)
      )
      val mockSaveBranchToRepo = fakeBranchRepoDir(_) = _
      val mockBranchContent = fakeBranchRepoDir(_)

      val newHead =
        head.update(
          "newCommitOnMaster",
          mockBranchContent,
          mockSaveBranchToRepo
        )

      fakeBranchRepoDir(newHead.content) shouldBe "newCommitOnMaster"
    }

    it("should update Head content when detached mode") {
      val head = Head("detached", "commit")
      val fakeBranchRepoDir = mutable.Map[String, String]()
      val mockSaveBranchToRepo = fakeBranchRepoDir(_) = _
      val mockBranchContent = fakeBranchRepoDir(_)

      val newHead =
        head.update(
          "newCommitOnMaster",
          mockBranchContent,
          mockSaveBranchToRepo
        )

      newHead.content shouldBe "newCommitOnMaster"
    }
  }

  describe("Head generating") {
    it("should extract head content from file") {
      val head = Head("branch", "master")

      Head.fromHeadFile(head.toXml()) shouldBe head
    }

    it("should create head content from branch if exists") {
      val branch = Branch("master", "commit")
      val expectedHead = Head("branch", "master")
      val fakeBranchRepoDir =
        mutable.Map[String, String]((branch.name, branch.commitHash))
      val mockBranchExists = fakeBranchRepoDir.contains(_)

      val res = Head.fromBranchName("master", mockBranchExists)

      assert(res.isDefined)
      res.get shouldBe expectedHead
    }

    it("should return None if branch does not exist") {
      val fakeBranchRepoDir =
        mutable.Map[String, String]()
      val mockBranchExists = fakeBranchRepoDir.contains(_)

      val res = Head.fromBranchName("master", mockBranchExists)

      res shouldBe None
    }

    it("should return head content from commit hash ") {
      Head.fromCommitHash("abc") shouldBe Head("detached", "abc")
    }

  }

  describe("Head exploration") {
    it("should return last commit in branch mode") {
      val head = Head("branch", "master")
      val commit = Commit("hash", "", "text", "author")
      val master = Branch("master", commit.hash)
      val fakeBranchRepoDir = mutable.Map[String, String](
        (master.name, master.commitHash)
      )
      val fakeCommitRepoDir =
        mutable.Map[String, Node]((commit.hash, commit.toXml()))
      val mockCommitContent = fakeCommitRepoDir(_)
      val mockBranchContent = fakeBranchRepoDir(_)

      val res = head.getLastCommit(mockCommitContent, mockBranchContent)

      assert(res.isDefined)
      res.get.hash shouldBe commit.hash
    }

    it("should return last commit in detached mode") {
      val commit = Commit("hash", "", "text", "author")

      val head = Head("detached", commit.hash)
      val fakeBranchRepoDir = mutable.Map[String, String]()
      val fakeCommitRepoDir =
        mutable.Map[String, Node]((commit.hash, commit.toXml()))
      val mockCommitContent = fakeCommitRepoDir(_)
      val mockBranchContent = fakeBranchRepoDir(_)

      val res = head.getLastCommit(mockCommitContent, mockBranchContent)

      assert(res.isDefined)
      res.get.hash shouldBe commit.hash
    }

    it("should return None if no last commit in detached mode") {
      val head = Head("detached", "")
      val fakeBranchRepoDir = mutable.Map[String, String]()
      val fakeCommitRepoDir =
        mutable.Map[String, Node]()
      val mockCommitContent = fakeCommitRepoDir(_)
      val mockBranchContent = fakeBranchRepoDir(_)

      val res = head.getLastCommit(mockCommitContent, mockBranchContent)

      res shouldBe None
    }

    it("should return None if no last commit in branch mode") {
      val head = Head("branch", "master")
      val master = Branch("master", "")
      val fakeBranchRepoDir =
        mutable.Map[String, String]((master.name, master.commitHash))
      val fakeCommitRepoDir =
        mutable.Map[String, Node]()
      val mockCommitContent = fakeCommitRepoDir(_)
      val mockBranchContent = fakeBranchRepoDir(_)

      val res = head.getLastCommit(mockCommitContent, mockBranchContent)

      res shouldBe None
    }
  }
}
