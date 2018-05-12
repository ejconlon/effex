val thisScalaVersion = "2.11.12"


name := "effect-extras"
version := "0.0.1"
organization in ThisBuild := "io.github.ejconlon"
scalaVersion in ThisBuild := thisScalaVersion


val commonSettings = Seq(
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:higherKinds", "-Ypartial-unification"),

  resolvers ++= Seq(
    "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Second Typesafe repo" at "http://repo.typesafe.com/typesafe/maven-releases/",
    Resolver.sonatypeRepo("public")
  )
)


val catsVersion = "1.1.0"
val possibleDeps = Map(
  "cats-core" -> "org.typelevel" %% "cats-core" % catsVersion,
  "cats-effect" -> "org.typelevel" %% "cats-effect" % "1.0.0-RC",
  "cats-free" -> "org.typelevel" %% "cats-free" % catsVersion,
  "scalatest" -> "org.scalatest" %% "scalatest" % "3.0.5"
//  "fastparse-byte" -> "com.lihaoyi" %% "fastparse-byte" % "1.0.0",
//  "kind-projector" -> "org.spire-math" %% "kind-projector" % "0.9.4",
//  "scala-reflect" -> "org.scala-lang" % "scala-reflect" % thisScalaVersion,
//  "scodec-core" -> "org.scodec" %% "scodec-core" % "1.10.3",
//  "scodec-bits" -> "org.scodec" %% "scodec-bits" % "1.1.5",
//  "shapeless" -> "com.chuusai" %% "shapeless" % "2.3.3"
)



val actualDeps = Seq(
  possibleDeps("cats-core"),
  possibleDeps("cats-effect"),
  possibleDeps("cats-free"),
  possibleDeps("scalatest") % "test"
)


val root = project
  .in(file("."))
  .settings(commonSettings, libraryDependencies ++= actualDeps)
