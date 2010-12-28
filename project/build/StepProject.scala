import sbt._

class HelloWorldProject(info: ProjectInfo) extends DefaultProject(info) {

  val mirror_repo = "Maven 2 mirror" at "http://mirrors.ibiblio.org/pub/mirrors/maven2/"

  override def mainClass = Some("calculator.Parser")

  override def libraryDependencies = Set(
    "ch.qos.logback" % "logback-classic" % "0.9.27"
  ) ++ super.libraryDependencies

}