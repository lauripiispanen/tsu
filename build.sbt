organization in GlobalScope := "rhurmala"

scalaVersion in GlobalScope := "2.10.3"

libraryDependencies in GlobalScope := Seq(
  "org.scalatest" %% "scalatest" % "2.0" % "test"
)

dependencyOverrides in GlobalScope := Set(
  "org.scala-lang" % "scala-library" % "2.10.3",
  "org.scala-lang" % "scala-reflect" % "2.10.3"
)

conflictManager in GlobalScope := ConflictManager.strict

resolvers in GlobalScope += "Sonatype" at "https://oss.sonatype.org/content/groups/public"

moduleName := "tsu-project"

name := "tsu-project"

version := "1"

lazy val `tsu` =
  project
    .in(file("tsu"))
    .settings(
      version := "1-SNAPSHOT"
    )

lazy val `tsu-parser` =
  project
    .in(file("tsu-parser"))
    .dependsOn(tsu)
    .settings(
      version := "1-SNAPSHOT"
    )

lazy val `tsu-argonaut` =
  project
    .in(file("tsu-argonaut"))
    .dependsOn(tsu)
    .settings(
      version := "1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "io.argonaut" %% "argonaut" % "6.0.3"
      )
    )

lazy val `tsu-jackson` =
  project
    .in(file("tsu-jackson"))
    .dependsOn(tsu)
    .settings(
      version := "1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "com.fasterxml.jackson.core" % "jackson-core" % "2.3.2"
      )
    )