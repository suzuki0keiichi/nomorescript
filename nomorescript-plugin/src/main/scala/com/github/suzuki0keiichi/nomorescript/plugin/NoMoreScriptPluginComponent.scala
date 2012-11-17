package com.github.suzuki0keiichi.nomorescript.plugin

import java.io.IOException
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptApply
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptCases
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptClass
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptConstructor
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptDef
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIf
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptInstanceOf
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptJsFunction
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptLiteral
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptNamespace
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptNew
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptOperator
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptSelect
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptSetter
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptThis
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptThrow
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrait
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTraitDef
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTree
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrees
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTry
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptVal
import java.io.File
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import com.github.suzuki0keiichi.nomorescript.converter.TreeConverter
import com.github.suzuki0keiichi.nomorescript.converter.ScopedVariables

class NoMoreScriptPluginComponent(val global: Global, parent: NoMoreScriptPlugin) extends PluginComponent with TreeConverter {

  import global._

  val runsAfter: List[String] = List("refchecks")
  val phaseName: String = "scala to javascript convert phase"
  val localUnit = new ThreadLocal[(CompilationUnit, Boolean)]
  def newPhase(prev: Phase) = new NoMoreScriptPhase(prev)
  def findClass(name: String): Option[ClassDef] = findClass(name, localUnit.get._1.body)

  def findClass(name: String, tree: Tree): Option[ClassDef] = {
    tree match {
      case cdef: ClassDef =>
        findConstructor(cdef) match {
          case Some(ddef) if (cdef.name.toString == name ||
            (ddef.tpt.toString.startsWith("anonymous class ") && ddef.tpt.toString.substring("anonymous class ".length) == name)) =>
            Some(cdef)

          case _ =>
            tree.children.map(findClass(name, _)).collectFirst {
              case Some(classDef) => classDef
            }
        }

      case _ =>
        tree.children.map(findClass(name, _)).collectFirst {
          case Some(classDef) => classDef
        }
    }
  }

  def addError(pos: Position, message: String) {
    localUnit.get()._1.error(pos, message)
    localUnit.set((localUnit.get()._1, true))
  }

  def hasError = localUnit.get._2

  class NoMoreScriptPhase(prev: Phase) extends StdPhase(prev) {
    override def name: String = phaseName

    def apply(unit: CompilationUnit) {
      localUnit.set((unit, false))

      val currentDir = new java.io.File(System.getProperties().getProperty("user.dir"))
      val scopedVars = new ScopedVariables(null)

      unit.body match {
        case pdef: PackageDef =>
          val js = toPackage(pdef, scopedVars).toJs(false)

          if (!hasError) {
            val writer = createWriter(currentDir, unit)

            try {
              js.foreach(writer.println(_))
              writer.flush()
            } catch {
              case e: IOException => addError(NoPosition, e.getMessage)
            } finally {
              writer.close()
            }
          }

        case t: Tree =>
          unit.error(t.pos, "not supported " + t.getClass)
      }
    }

    private def createWriter(currentDir: java.io.File, unit: CompilationUnit) = {
      val file = unit.source.file.file
      val srcRootDir = {
        val file = new File(parent.srcDir)
        if (file.isAbsolute()) {
          file.getAbsolutePath()
        } else {
          global.settings.outputDirs.outputs.headOption match {
            case Some(dirPair) => dirPair._1.path + File.separator + parent.srcDir
            case None => unit.source.file.file.getParent() + File.separator + parent.srcDir
          }
        }
      }

      val fileParent = {
        if (!file.getParent.endsWith(java.io.File.separator)) {
          file.getParent + java.io.File.separator
        } else {
          file.getParent
        }
      }

      val relativePath =
        if (fileParent.startsWith(srcRootDir)) {
          fileParent.substring(srcRootDir.length())
        } else {
          ""
        }

      val outputDirRoot = {
        val file = new java.io.File(parent.outputDir)

        if (file.isAbsolute) {
          file.getAbsolutePath
        } else {
          global.settings.outputDirs.outputs.headOption match {
            case Some(dirPair) => dirPair._2 + File.separator + file.getPath()
            case None => currentDir.getAbsolutePath + File.separator + file.getPath
          }
        }
      }

      val outputDir = new java.io.File(outputDirRoot + File.separator + relativePath)

      outputDir.mkdirs()

      new java.io.PrintWriter(outputDir.getPath + File.separator + file.getName.replaceAll(".scala", "") + ".js", "UTF-8")
    }
  }
}
