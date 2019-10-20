package igpolytech
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.collection.mutable
import scala.xml.Node

class BranchTest extends FunSpec with Matchers {
  describe("Branch generating") {
    it("should create branch if exists") {
      val branch = Branch("a", "b")
      val fakeBranchRepoDir =
        mutable.Map[String, String]((branch.name, branch.commitHash))

      val res = Branch.getBranchOption(
        "a",
        (branchName: String) => fakeBranchRepoDir.contains(branchName),
        (branchName: String) => fakeBranchRepoDir(branchName)
      )

      assert(res.isDefined)
      res.get.name shouldBe branch.name
      res.get.commitHash shouldBe branch.commitHash
    }

    it("should return None if branch does not exists") {
      val branch =
        Branch.getBranchOption("name", (_) => false, (_) => "")

      branch shouldBe None
    }
  }

  describe("Branch saving") {
    it("should save in repo dir") {
      val fakeBranchRepoDir = mutable.Map[String, String]()
      val mockSaveToRepo = fakeBranchRepoDir(_) = _
      val branch = Branch("master", "commitHash")

      branch.save(mockSaveToRepo)

      assert(fakeBranchRepoDir.contains("master"))
      fakeBranchRepoDir.get("master").get shouldBe "commitHash"
    }

    it("should update branch with new commit hash") {
      val branch = Branch("master", "commitHash")

      val updated = branch.update("newCommitHash")

      updated.commitHash shouldBe "newCommitHash"
    }
  }

  describe("Branch exploring") {
    it("should return last commit if some") {
      val commit = Commit("hash", "", "text", "author")
      val fakeCommitRepoDir =
        mutable.Map[String, Node]((commit.hash, commit.toXml()))
      val mockCommitContent = fakeCommitRepoDir(_)
      val branch = Branch("master", commit.hash)

      val res = branch.getLastCommit(mockCommitContent)

      assert(res.isDefined)
      res.get shouldBe commit
    }

    it("should return None if no last commit") {
      val branch = Branch("master", "")
      val fakeCommitRepoDir = mutable.Map[String, Node]()
      val mockCommitContent = (hash: String) => fakeCommitRepoDir(hash)

      val res = branch.getLastCommit(mockCommitContent)

      res shouldBe None
    }
  }
}
