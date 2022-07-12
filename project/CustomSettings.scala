import sbt._
import sbt.Keys._

object CustomSettings {

  val scalaVersion_213  = "2.13.8"

  val scala213Config = Seq(scalaVersion := scalaVersion_213, scalacOptions ++= Seq("-feature", "-deprecation"))

  val fmtConfig = org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile := true

  val scala31Settings  = fmtConfig +: scala213Config

}
