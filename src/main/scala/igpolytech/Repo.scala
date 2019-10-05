package igpolytech
import java.io.File

class Repo {
  def init(path: String): String = {
    val sgitDir = s"${path}${File.separator}.sgit${File.separator}"
    val dirs: Array[String] = Array(
      s"${sgitDir}tags",
      s"${sgitDir}commits",
      s"${sgitDir}trees",
      s"${sgitDir}blobs",
      s"${sgitDir}branches"
    )
    val files: Array[String] = Array(s"${sgitDir}STAGE", s"${sgitDir}HEAD");
    if (!FilesIO.dirExists(sgitDir) && !FilesIO.parentDirExists(sgitDir)) {
      try {
        FilesIO.createDirectories(dirs);
        FilesIO.createFiles(files);
        return "Repo initialized"
      } catch {
        case e: Exception => e.getMessage();
      }
    } else {
      "Sorry, a sgit repository is already initialized";
    }
  }
}
