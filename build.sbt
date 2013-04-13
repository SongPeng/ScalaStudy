name := "ScalaStudy"

description := "SBT sucks"

version := "0.1"

scalaVersion := "2.10.1"

resolvers ++= Seq(
    "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
    "spring milestone" at "https://repo.springsource.org/libs-milestone",
    "io.spary" at "http://repo.spray.io",
    DefaultMavenRepository
)

libraryDependencies ++= Seq(
)
