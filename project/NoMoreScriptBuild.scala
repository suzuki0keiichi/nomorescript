import sbt._
import Keys._

object NoMoreScriptBuild extends Build {
  lazy val root = Project(id = "nomorescript", base = file(".")) aggregate(plugin, browser, jquery, enchantjs)

  lazy val plugin = Project(id = "nomorescript-plugin", base = file("nomorescript-plugin"))

  lazy val browser = Project(id = "nomorescript-option-browser", base = file("nomorescript-option-browser")) dependsOn(plugin)

  lazy val jquery = Project(id = "nomorescript-option-jquery", base = file("nomorescript-option-jquery")) dependsOn(plugin)

  lazy val enchantjs = Project(id = "nomorescript-option-enchantjs", base = file("nomorescript-option-enchantjs")) dependsOn(plugin)
}
