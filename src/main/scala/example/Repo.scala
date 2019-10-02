package example

class Repo {
  def init() : Option[String] = {
    val dirs : Array[String] = Array(".sgit/tags", ".sgit/commits", ".sgit/trees", ".sgit/blobs", ".sgit/branches");
    val files : Array[String] = Array(".sgit/STAGE", ".sgit/HEAD");
    if(!FilesIO.dirExists(".sgit")) {
      //TODO AND SHOULD NOT BE PRESENT IN UPPER DIR
      try {
        FilesIO.createDirectories(dirs);
        FilesIO.createFiles(files);
        return None;
      } catch {
        case e : Exception => Some(e.getMessage());
      }
    } else {
      Some("Sorry, a sgit repository is already initialized");
    }
  }
}