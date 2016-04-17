import sbt._
import sbt.Keys._

import java.io.{PrintWriter}

object FunctionalJs extends Build {
  lazy val functionalJsProject = Project(
    id = "functional-js",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Functional Book Test Project",
      organization := "akimichi.tatsukawa",
      version := "0.1-SNAPSHOT",
      // scalaVersion := "2.12",
      // scalaVersion := "2.10",
      scalaVersion := "2.11.1",
      // scalaVersion := "2.11.7",
      // scalaVersion := "2.9.1",

      // add other settings here
      /** scalacOptions
       *  -unchecked
       *  -deprecation
       *  -Xprint:typer 暗黙変換の情報を表示する
       *  -Xcheckinit   継承関係における予測しがたいスーパークラスの初期化をチェックする
       */
      // scalacOptions ++= Seq("-unchecked", "-deprecation","-Xcheckinit", "-P:continuations:enable"),
      scalacOptions ++= Seq("-unchecked", "-deprecation","-Xcheckinit"),
      scalacOptions in Test ++= Seq("-Yrangepos"),
        // scalacOptions ++= Seq("-unchecked", "-deprecation","-Xcheckinit"),
      resolvers ++= Seq("Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
        "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
        "releases"  at "http://oss.sonatype.org/content/repositories/releases",
        "Maven Repository" at "http://repo1.maven.org/maven2/",
        // "Artima Maven Repository" at "http://repo.artima.com/releases",
        "repo.novus rels" at "http://repo.novus.com/releases/",
        "repo.novus snaps" at "http://repo.novus.com/snapshots/"),
      libraryDependencies ++= Seq (
        "junit" % "junit" % "4.10" % "test",
        "com.novocode" % "junit-interface" % "0.10-M2" % "test",
        "org.scalactic" %% "scalactic" % "2.2.6",
        "org.scalatest" %% "scalatest" % "2.2.6" % "test",
        // "org.scalatest" %% "scalatest" % "2.0" % "test",
        // "org.scalatest" %% "scalatest" % "1.7.1" % "test",
        "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
        "org.specs2" %% "specs2-core" % "3.7.2" % "test"
          // "org.specs2" %% "specs2" % "1.12.1" % "test"
      )
    )
  )
}
