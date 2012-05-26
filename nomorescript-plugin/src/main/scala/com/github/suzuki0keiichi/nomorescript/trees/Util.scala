package com.github.suzuki0keiichi.nomorescript.trees

object Util {
  def addLast(lines: List[String], text: String): List[String] = {
    if (lines.isEmpty) {
      List(text)
    } else {
      if (lines.size > 1) {
        lines.init ::: List(lines.last + text)
      } else {
        List(lines(0) + text)
      }
    }
  }

  def addFirst(lines: List[String], text: String): List[String] = {
    if (lines.isEmpty) {
      List(text)
    } else {
      if (lines.size > 1) {
        List(text + lines.first) ::: lines.tail
      } else {
        List(text + lines(0))
      }
    }
  }
}