organization in GlobalScope := "rhurmala"

scalaVersion in GlobalScope := "2.11.0"

crossScalaVersions := Seq("2.10.3", "2.11.0")

libraryDependencies in GlobalScope := Seq(
  "org.scalatest" %% "scalatest" % "2.1.7" % "test"
)

dependencyOverrides in GlobalScope := Set(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
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
      version := "1-SNAPSHOT",
      libraryDependencies <++= scalaVersion({
        case "2.11.0" => Seq("org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4")
        case _ => Seq()
      })
    )

lazy val `tsu-argonaut` =
  project
    .in(file("tsu-argonaut"))
    .dependsOn(tsu)
    .settings(
      version := "1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "io.argonaut" %% "argonaut" % "6.0.4"
      )
    )

lazy val `tsu-spray` =
  project
    .in(file("tsu-spray"))
    .dependsOn(tsu)
    .settings(
      version := "1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "io.spray" %% "spray-json" % "1.2.6"
      )
    )
lazy val `tsu-json4s` =
  project
    .in(file("tsu-json4s"))
    .dependsOn(tsu)
    .settings(
      version := "1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "org.json4s" %% "json4s-ast" % "3.2.10",
        "org.json4s" %% "json4s-core" % "3.2.10"
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
