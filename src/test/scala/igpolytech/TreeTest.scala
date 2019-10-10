package igpolytech
import java.security.MessageDigest
import org.scalatest._
import org.scalatest.Matchers
import scala.xml.Node
import java.io.File

class TreeTest extends FunSpec with Matchers {
  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File("testDir").exists()) FilesIO.delete("testDir")
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
      val emptyTree = new Tree("A113")
      val emptyTree2 = new Tree("A114")
      val treeWithTree = new Tree("A114", Array(emptyTree2))
      val treeWithBlob = new Tree(
        "A114",
        Array(emptyTree2),
        Array(new Blob("unoBlobo", () => "Italian"))
      )
      emptyTree.hash shouldNot be(emptyTree2.hash)
      emptyTree2.hash shouldNot be(treeWithTree.hash)
      treeWithTree.hash shouldNot be(treeWithBlob.hash)
    }

    it("should create tree from string array") {
      FilesIO.createDirectories(Array("testDir"))
      val fakeProjectDir = new File(".").getCanonicalPath()
      FilesIO.write("testDir/test", "abc")

      val createdTree =
        Tree.createFromList(
          Array("testDir"),
          s"${fakeProjectDir}${File.separator}"
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
      val tree = new Tree("A113")
      val blob = new Blob("unoBlobo", () => "Italian")
      val blob2 = new Blob("duoBlobi", () => "Italian again")
      val treeWithBlob = new Tree("A114", Array(tree), Array(blob))
      val treeWithBlob2 = new Tree(
        "A114",
        Array(tree),
        Array(blob2)
      )

      val mergedTree = treeWithBlob.mergeTree(treeWithBlob2)

      val expectedTree = new Tree("A114", Array(tree), Array(blob, blob2))
      assert(mergedTree.equals(expectedTree))
    }
  }

  describe("Tree exploration") {
    it("should return all file from a tree") {
      val firstTree =
        new Tree("A114", Array(), Array(new Blob("unoBlobo", () => "Italian")))
      val secondTree =
        new Tree("A115", Array(), Array(new Blob("duoBlobi", () => "Italian")))
      val tree = new Tree(
        "A113",
        Array(firstTree, secondTree)
      )

      val allFiles = tree.getAllFiles()

      assert(
        allFiles.contains(s"A113${File.separator}A114${File.separator}unoBlobo")
      )
      assert(
        allFiles.contains(s"A113${File.separator}A115${File.separator}duoBlobi")
      )
    }

    // describe("Tree diff") {
    //   it("should return no diff between the same files") {
    //     FilesIO.createDirectories(Array("testDir"))
    //     FilesIO.write("testDir/test", "abc")

    //     val diff = Diff.fromFiles("testDir/test", "testDir/test")

    //     assert(diff.isEmpty)
    //   }

    //   it("should return the difference between two files") {
    //     FilesIO.createDirectories(Array("testDir"))
    //     FilesIO.write("testDir/old", "to infinity")
    //     FilesIO.write("testDir/new", "to infinity\nand beyond")

    //     val diff = Diff.fromFiles("testDir/old", "testDir/new")

    //     val expectedDiff = new Diff(ChangeType.ADD, "and beyond")
    //     assert(diff.isDefined)
    //     assert(diff.equals(expectedDiff))
    //   }

    //   it("should return the diff between two different trees") {
    //     val firstTree =
    //       new Tree(
    //         "A113",
    //         Array(),
    //         Array(new Blob("file", () => "to infinity"))
    //       )
    //     val secondTree =
    //       new Tree(
    //         "A113",
    //         Array(),
    //         Array(new Blob("file", () => "to infinity\nand beyond"))
    //       )

    //     val diffArray = Diff.fromTrees(firstTree, secondTree)

    //     val expectedDiff = new Diff(ChangeType.ADD, "and beyond")
    //     assert(!diffArray.isEmpty)
    //     assert(diffArray.length == 1)
    //     assert(diffArray.contains(expectedDiff))
    //   }

    //   it("should return no diff between the same trees") {
    //     val firstTree =
    //       new Tree(
    //         "A113",
    //         Array(),
    //         Array(new Blob("unoBlobo", () => "Italian"))
    //       )

    //     val diffArray = Diff.fromTrees(firstTree, firstTree)

    //     assert(diffArray.isEmpty)
    //   }
    // }
  }
}
