package igpolytech
import org.scalatest._
import org.scalatest.Matchers
import java.io.File
import scala.xml.Node
import scala.collection.mutable

class TreeTest extends FunSpec with Matchers {
  describe("Tree generating") {
    it("should generate hash base on concatenation of name trees and blobs") {
      val emptyTree = new Tree(s"A113${File.separator}")
      val emptyTree2 = new Tree(s"A114${File.separator}")
      val treeWithTree = new Tree(s"A114${File.separator}", Array(emptyTree2))
      val treeWithBlob = new Tree(
        s"A114${File.separator}",
        Array(emptyTree2),
        Array(new Blob("unoBlobo", () => "Italian"))
      )

      emptyTree.hash shouldNot be(emptyTree2.hash)
      emptyTree2.hash shouldNot be(treeWithTree.hash)
      treeWithTree.hash shouldNot be(treeWithBlob.hash)
    }

    it("should generate tree from hash") {
      val blob = Blob("unoBlobo", () => "Italian")
      val emptyTree2 = new Tree(s"A114${File.separator}nothing")
      val tree = Tree(
        s"A114${File.separator}",
        Array(emptyTree2),
        Array(blob)
      )
      val fakeRepoTreeDir = mutable.Map[String, Node](
        (tree.hash, tree.toXml()),
        (emptyTree2.hash, emptyTree2.toXml())
      )
      val fakeRepoBlobDir =
        mutable.Map[String, String]((blob.hash, blob.content))
      val mockBlobContent = (hash: String) => fakeRepoBlobDir.get(hash).get
      val mockTreeContent = (hash: String) => fakeRepoTreeDir.get(hash).get

      val res = Tree.getTree(tree.hash, mockBlobContent, mockTreeContent)

      res.hash shouldBe tree.hash
    }

    it("should generate tree from file of working dir") {
      val blob = Blob("unoBlobo", () => "Italian")
      val blob2 = Blob("duoBlobi", () => "Italian")
      val fakeWorkingDir =
        mutable.Map[String, String](
          ("A113/unoBlobo", blob.content),
          ("A113/A114/duoBlobi", blob2.content)
        )
      val subTree = Tree("A113/A114/", Array(), Array(blob2))
      val tree = Tree(
        "A113/",
        Array(subTree),
        Array(blob)
      )
      val mockFileContent = (dirName: String) =>
        (fileName: String) => fakeWorkingDir(s"${dirName}${fileName}")

      val res = Tree.createTree(
        Array(Array("unoBlobo"), Array("A114", "duoBlobi")),
        "A113/",
        "",
        mockFileContent
      )

      res.hash shouldBe tree.hash
    }

    it("should save blob and tree in repo dir") {
      val fakeRepoTreeDir = mutable.Map[String, Node]()
      val fakeRepoBlobDir = mutable.Map[String, String]()
      val mockSaveTree =
        (xml: Node, hash: String) => fakeRepoTreeDir(hash) = xml
      val mockSaveBlob =
        (hash: String, content: String) => fakeRepoBlobDir(hash) = content
      val blob = Blob("b", () => "test1")
      val blob2 = Blob("a", () => "test")
      val tree =
        Tree(
          s"A114${File.separator}",
          Array(),
          Array(blob)
        )
      val rootTree =
        Tree(
          "",
          Array(tree),
          Array(blob2)
        )

      rootTree.save(mockSaveTree, mockSaveBlob)

      assert(fakeRepoBlobDir.contains(blob.hash))
      assert(fakeRepoBlobDir.contains(blob2.hash))
      assert(fakeRepoTreeDir.contains(tree.hash))
      assert(fakeRepoTreeDir.contains(rootTree.hash))

    }

    describe("Tree merging") {
      it("should merge two trees") {
        val tree = new Tree(s"A113${File.separator}")
        val blob = new Blob("unoBlobo", () => "Italian")
        val blob2 = new Blob("duoBlobi", () => "Italian again")
        val treeWithBlob =
          new Tree(s"A114${File.separator}", Array(tree), Array(blob))
        val treeWithBlob2 = new Tree(
          s"A114${File.separator}",
          Array(tree),
          Array(blob2)
        )

        val mergedTree = treeWithBlob.mergeTree(treeWithBlob2)

        val expectedTree =
          new Tree(s"A114${File.separator}", Array(tree), Array(blob, blob2))
        assert(mergedTree.equals(expectedTree))
      }
    }

    describe("Tree exploration") {
      it("should return all files from a Tree") {
        val tree =
          Tree(
            s"A114${File.separator}",
            Array(),
            Array(Blob("b", () => "test"))
          )
        val rootTree =
          Tree(
            "",
            Array(tree),
            Array(Blob("a", () => "test"))
          )

        val res = rootTree.getAllFiles()

        assert(res.contains("A114/b"))
        assert(res.contains("a"))
      }

      it("should give the difference between stored blob and working dir") {
        val blob = Blob("b", () => "testo")
        val subTree = Tree("sub")
        val tree =
          Tree(
            s"A114${File.separator}",
            Array(subTree),
            Array(blob)
          )
        val fakeRepoBlobDir =
          mutable.Map[String, String](
            (blob.hash, blob.content)
          )
        val fakeWorkingDir = mutable.Map[String, String](
          (blob.name, "testo\nplusSomeContent")
        )
        val mockBlobContent = (hash: String) => fakeRepoBlobDir.get(hash).get
        val mockBlobExists = (a: String) => (b: String) => true
        val mockFileContent =
          (a: String) => (name: String) => fakeWorkingDir.get(name).get

        val res =
          tree
            .getModified(mockBlobExists, mockBlobContent, mockFileContent)

        assert(res.length > 0)
        assert(
          res(0).changes.contains(Change(ChangeType.ADD, "plusSomeContent", 2))
        )
      }

      it("should create all file from a Tree") {

        val fakeWorkingDir = mutable.Map[String, String]()
        val mockWrite = (tree: String) =>
          (hash: String, content: String) =>
            fakeWorkingDir(s"${tree}$hash") = content

        val blob = Blob("b", () => "testo")
        val blob2 = new Blob("a", () => "testi")
        val subTree = Tree(s"A114/sub/", Array(), Array(blob2))
        val tree =
          Tree(
            s"A114${File.separator}",
            Array(subTree),
            Array(blob)
          )

        tree.createAllFiles(mockWrite)

        assert(fakeWorkingDir.contains(s"${tree.name}${blob.name}"))
        assert(fakeWorkingDir.contains(s"${subTree.name}${blob2.name}"))

      }
    }

    describe("Tree diff") {
      it("should return no diff between the same content") {
        val diff = Diff.fromContents("test\ntest2", "test\ntest2", "")

        assert(diff.isEmpty)
      }

      it("should return the diff between different content") {
        val diff = Diff.fromContents(
          "It's known\nDisney is best",
          "It's known\nPixar is best",
          ""
        )

        assert(diff.isDefined)
        assert(diff.get.changes.length == 2)
        assert(
          diff.get.changes.contains(Change(ChangeType.ADD, "Pixar is best", 2))
        )
        assert(
          diff.get.changes.contains(Change(ChangeType.SUB, "Disney is best", 2))
        )
      }
    }
  }
}
