package com.github.suzuki0keiichi.nomorescript.converter

object BaseClasses  {
  private val classNames = List("java.lang.Object", "ScalaObject", "Product", "Serializable", "Object")
  
  def isBaseClass(name: String) = classNames.contains(name)
}