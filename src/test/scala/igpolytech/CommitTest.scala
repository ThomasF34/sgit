package igpolytech
import org.scalatest.FunSpec
import org.scalatest.Matchers
import java.io.File
import scala.collection.mutable
import scala.xml.Node

class CommitTest extends FunSpec with Matchers {
  describe("Commit saving") {
    it("should save commit to repo dir") {
      val commit = Commit("hash", "", "text", "author")
      val fakeCommitRepoDir =
        mutable.Map[String, Node]((commit.hash, commit.toXml()))
      val mockSaveCommit =
        (content: Node, hash: String) => fakeCommitRepoDir(hash) = content

      commit.save(mockSaveCommit)

      assert(fakeCommitRepoDir.contains(commit.hash))
      fakeCommitRepoDir(commit.hash) shouldBe commit.toXml()
    }

    it("should say if commit exists") {
      val commit = Commit("hash", "", "text", "author")
      val fakeCommitRepoDir =
        mutable.Map[String, Node]((commit.hash, commit.toXml()))
      val mockCommitExists = fakeCommitRepoDir.contains(_)

      Commit.exists(commit.hash, mockCommitExists) shouldBe true
      Commit.exists("test", mockCommitExists) shouldBe false
    }
  }

  describe("Commit exploration") {
    it("should get common ancestor") {
      val root = Commit("a", "", "text", "author")
      val ancestor = Commit("common", root.hash, "text", "author")
      val branch1 = Commit("treeForBranch1", ancestor.hash, "text", "author")
      val branch2 = Commit("treeForBranch2", ancestor.hash, "text", "author")

      val fakeCommitRepoDir =
        mutable.Map[String, Node](
          (root.hash, root.toXml()),
          (ancestor.hash, ancestor.toXml()),
          (branch2.hash, branch2.toXml()),
          (branch1.hash, branch1.toXml())
        )
      val mockCommitExists = fakeCommitRepoDir.contains(_)
      val mockCommitContent = fakeCommitRepoDir(_)

      val res = Commit.getAncestorCommit(
        branch1,
        branch2,
        mockCommitContent,
        mockCommitExists
      )

      assert(res.isDefined)
      res.get.hash shouldBe ancestor.hash
    }

    it("should return None if no common ancestor") {
      val root = Commit("a", "", "text", "author")
      val root2 = Commit("b", "", "text", "author")
      val branch1 = Commit("treeForBranch1", root.hash, "text", "author")
      val branch2 = Commit("treeForBranch2", root2.hash, "text", "author")

      val fakeCommitRepoDir =
        mutable.Map[String, Node](
          (root.hash, root.toXml()),
          (root2.hash, root2.toXml()),
          (branch2.hash, branch2.toXml()),
          (branch1.hash, branch1.toXml())
        )
      val mockCommitExists = fakeCommitRepoDir.contains(_)
      val mockCommitContent = fakeCommitRepoDir(_)

      val res = Commit.getAncestorCommit(
        branch1,
        branch2,
        mockCommitContent,
        mockCommitExists
      )

      res shouldBe None
    }
  }
  describe("Commit creation") {
    it("should trim text as title if too long") {
      val commit = Commit(
        "",
        "",
        "This text is very long and should be cut cause it will be way too long as a title don't you thing ?",
        "author"
      )

      commit.title shouldBe "This text is very long and should be cut cause it w"
    }

    it("should not trim text as title if short") {
      val commit = Commit(
        "",
        "",
        "Short text is great as title",
        "author"
      )

      commit.title shouldBe "Short text is great as title"
    }
  }

  describe("Commit exploration") {
    it("should return commit if exists") {
      val commit = Commit("treeForBranch2", "", "text", "author")

      val fakeCommitRepoDir =
        mutable.Map[String, Node](
          (commit.hash, commit.toXml())
        )
      val mockCommitExists = fakeCommitRepoDir.contains(_)
      val mockCommitContent = fakeCommitRepoDir(_)

      val res =
        Commit.getCommitOption(commit.hash, mockCommitContent, mockCommitExists)

      assert(res.isDefined)
      res.get.hash shouldBe commit.hash
    }

    it("should return None if no commit") {
      val fakeCommitRepoDir =
        mutable.Map[String, Node](
          )
      val mockCommitExists = fakeCommitRepoDir.contains(_)
      val mockCommitContent = fakeCommitRepoDir(_)

      val res =
        Commit.getCommitOption("test", mockCommitContent, mockCommitExists)

      res shouldBe None
    }

  }

  implicit val ioManager = IOManager()
  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File(".sgit").exists()) ioManager.delete(".sgit")
    }
  }

  describe("Commit Integration tests") {
    it(
      "should ask for completion of CommitMessage file if no message is given as option"
    ) {
      Repo.init(".")
      val repo = Repo(".sgit")
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob = new Blob("A113", getFakeContent)
      val tree = new Tree("", Array(), Array(blob))
      repo.setStage(tree)

      val res = repo.commit("")

      assert(
        res.contains(
          "Please edit the commit message file with the commit message and use the commit command again"
        )
      )
    }

    it("should change HEAD branches when commiting") {
      Repo.init(".")
      val repo = Repo(".sgit")
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob = new Blob("A113", getFakeContent)
      val tree = new Tree("", Array(), Array(blob))
      repo.setStage(tree)

      val res = repo.commit("John Lasseter is great")

      val commitOption = repo.getLastCommit()
      val branchName = repo.head.content
      val commitOptionFromBranch = Branch
        .fromBranchName(branchName, repo.branchContent)
        .getLastCommit(repo.commitContent)
      assert(commitOptionFromBranch.isDefined)
      assert(commitOption.isDefined)
      assert(
        res.contains(
          "Change commited : John Lasseter is great"
        )
      )
      commitOption.get.text shouldBe "John Lasseter is great"
      commitOptionFromBranch.get.hash shouldBe commitOption.get.hash
    }

    it("should not try to commit if nothing is to be commited") {
      Repo.init(".")
      val repo = Repo(".sgit")

      val res = repo.commit("John Lasseter is great")

      res shouldBe "Sorry, nothing is to be commited. Use sgit add before commiting"

    }
  }
}
