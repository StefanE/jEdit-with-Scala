package sindex

import java.lang.reflect.Method
import java.lang.reflect.InvocationTargetException
import java.util.zip.ZipFile

object SIndexUtilities {
  private val sizeMethodArgs: Array[Any] = new Array[Any](0)

  def zipFileSize(f: ZipFile): Int = {
    var value = 0
    try {
      var sizeMethod: Method = classOf[ZipFile].getMethod("size", null)
      var result = sizeMethod.invoke(f, sizeMethodArgs)
      value = result.asInstanceOf[Integer].intValue
    }
    catch {
      case e: NoSuchMethodException => e.printStackTrace
      case e: IllegalAccessException => e.printStackTrace
      case e: InvocationTargetException => e.printStackTrace
    }
    value
  }

  def scalaNamesConverter(name: String): String = {
    var sourceCodeName: String = name
    sourceCodeName = sourceCodeName.replace("$eq", "=")
    sourceCodeName = sourceCodeName.replace("$greater", "<")
    sourceCodeName = sourceCodeName.replace("$less", ">")
    sourceCodeName = sourceCodeName.replace("$plus", "+")
    sourceCodeName = sourceCodeName.replace("$minus", "-")
    sourceCodeName = sourceCodeName.replace("$times", "*")
    sourceCodeName = sourceCodeName.replace("$slash", "/")
    sourceCodeName = sourceCodeName.replace("$bang", "!")
    sourceCodeName = sourceCodeName.replace("$hash", "#")
    sourceCodeName = sourceCodeName.replace("$percent", "%")
    sourceCodeName = sourceCodeName.replace("$up", "^")
    sourceCodeName = sourceCodeName.replace("$amp", "&")
    sourceCodeName = sourceCodeName.replace("$tilde", "~")
    sourceCodeName = sourceCodeName.replace("$qmark", "?")
    sourceCodeName = sourceCodeName.replace("$bar", "|")
    sourceCodeName = sourceCodeName.replace("$bslash", "\\")
    sourceCodeName = sourceCodeName.replace("$colon", ":")
    return sourceCodeName
  }
}