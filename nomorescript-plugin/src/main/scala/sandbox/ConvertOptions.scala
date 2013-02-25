package sandbox

import java.io.File
import scala.tools.nsc.settings.MutableSettings

case class ConvertOptions(moduleType: ModuleType, srcDirName: String = "", outputDirName: String = ".." + File.separator + "js") {
  def createWriter(currentDir: File, srcFile: File, settings: MutableSettings) = {
    val file = srcFile
    val srcRootDir = {
      val file = new File(srcDirName)
      if (file.isAbsolute()) {
        file.getAbsolutePath()
      } else {
        settings.outputDirs.outputs.headOption match {
          case Some(dirPair) => dirPair._1.path + File.separator + srcDirName
          case None => srcFile.getParent() + File.separator + srcDirName
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
      val file = new File(outputDirName)

      if (file.isAbsolute) {
        file.getAbsolutePath
      } else {
        settings.outputDirs.outputs.headOption match {
          case Some(dirPair) => dirPair._2 + File.separator + file.getPath()
          case None => currentDir.getAbsolutePath + File.separator + file.getPath
        }
      }
    }

    val outputDir = new File(outputDirRoot + File.separator + relativePath)

    outputDir.mkdirs()

    new java.io.PrintWriter(outputDir.getPath + File.separator + file.getName.replaceAll(".scala", "") + ".js", "UTF-8")
  }

}
