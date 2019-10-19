package igpolytech
import org.scalatest._
import org.scalatest.Matchers
import java.io.File

class TreeTest extends FunSpec with Matchers {
  implicit val ioManager = IOManager()
  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File("testDir").exists()) ioManager.delete("testDir")
    }
  }

  describe("Tree generating") {
    // it(
    //   "should generate a XML element based on containing trees, blobs and own name"
    // ) {
    //   val expectedXml: Node =
    //     <Tree>
    //       <name>A113</name>
    //       <trees>

    //       </trees>
    //       <blobs>

    //       </blobs>
    //      </Tree>
    //   assert(new Tree("A113").toXml().equals(expectedXml))
    // }

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

    it("should create tree from string array") {
      ioManager.createDirectories(Array("testDir"))
      val fakeProjectDir =
        s"${new File(".").getCanonicalPath()}${File.separator}"
      ioManager.write(s"testDir${File.separator}")("test", "abc")

      val allFiles = (f: File) => ioManager.getAllFiles(f)
      val fileContent = (dirName: String) =>
        (fileName: String) =>
          ioManager.getContent(s"${fakeProjectDir}${dirName}")(fileName)

      val createdTree =
        Tree.createFromList(
          Array("testDir"),
          fakeProjectDir,
          allFiles,
          fileContent,
          ioManager.fileExists
        )

      assert(
        createdTree.trees.map(_.name).contains(s"testDir${File.separator}")
      )

      assert(
        createdTree.trees.flatMap(_.blobs.map(_.name)).contains("test")
      )
    }
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
    it("should return all file from a tree") {
      val firstTree =
        new Tree(
          s"A113${File.separator}",
          Array(),
          Array(new Blob("unoBlobo", () => "Italian"))
        )
      val secondTree =
        new Tree(
          s"A114${File.separator}",
          Array(),
          Array(new Blob("duoBlobi", () => "Italian"))
        )
      val tree = new Tree(
        "",
        Array(firstTree, secondTree)
      )

      val allFiles = tree.getAllFiles()

      assert(
        allFiles.contains(s"A113${File.separator}unoBlobo")
      )
      assert(
        allFiles.contains(s"A114${File.separator}duoBlobi")
      )
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
