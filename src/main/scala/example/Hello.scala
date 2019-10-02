package example

object Hello extends Greeting with App {
  val action: String = "init";

  action match {
    case "init" => {
      new Repo().init() match {
        case Some(s) => println(s)
        case None => println("Repo initialized")
      }
    }
    case _ => println("Usage wesh")
  }
}

trait Greeting {
  lazy val greeting: String = "hello"
}
