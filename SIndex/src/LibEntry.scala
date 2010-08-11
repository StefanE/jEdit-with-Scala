package sindex

import java.util.zip.ZipFile
import java.io.IOException
import reflect.BeanProperty

object LibEntry {
  val PUBLIC = 1
  val PROTECTED = 2
  val PACKAGE = 3
  val PRIVATE = 4
}

class LibEntry(
        @BeanProperty
        var lib: String = "",
        @BeanProperty
        var doc: String = "",
        @BeanProperty
        var isOldJavaDoc: Boolean = false,
        @BeanProperty
        var visibility: Int = LibEntry.PROTECTED) {
  private var f: ZipFile = null
  private var oldlib = "$$$"

  def getLibFile: ZipFile = {
    if (f == null || !lib.equals(oldlib)) {
      if (f != null) {
        try {f.close}
        catch {case e: IOException => {}}
      }
      try {
        f = new ZipFile(lib)
        oldlib = lib
      }
      catch {case e: IOException => {f = null}}
    }
    f
  }

  protected override def finalize {if (f != null) f.close}

  override def toString = doc + " oldjdoc=" + isOldJavaDoc
}