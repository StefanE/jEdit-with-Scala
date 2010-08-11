/*
 * jEdit editor settings:
 * :mode=java:tabSize=4:indentSize=4:noTabs=true:maxLineLen=0:
 *
 * ZIPClassLoader.java - Loads classes from ZIP or JAR files
 * Copyright (C) 1999 Dirk Moebius (Modified by Stefan Ettrup)
 * Portions copyright (C) 1999 Slava Pestov, mike dillon
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package sindex

import java.io._
import java.net._
import java.util.zip._
import org.gjt.sp.jedit.MiscUtilities
import org.gjt.sp.jedit.jEdit

class ZIPClassLoader(val zipFile: ZipFile) extends ClassLoader {
  override def loadClass(clazzname: String): Class[_] = {
    return loadClass(clazzname, true)
  }

  override def loadClass(clazzname: String, resolveIt: Boolean): Class[_] = {
    var cls: Class[_] = findLoadedClass(clazzname)
    if (cls != null) {
      if (resolveIt) resolveClass(cls)
      return cls
    }

    val loader: ClassLoader = getClass.getClassLoader
    if (loader != null) {
      try {
        cls = loader.loadClass(clazzname)
        if (cls != null) {
          if (resolveIt) resolveClass(cls)
          return cls
        }
      }
      catch {
        case e: ClassNotFoundException => {}
      }
    }

    try {
      cls = findSystemClass(clazzname)
      if (cls != null) {
        if (resolveIt) resolveClass(cls)
        return cls
      }
    }
    catch {
      case e: ClassNotFoundException => {}
    }

    val filename: String = MiscUtilities.classToFile(clazzname)

    try {
      var entry: ZipEntry = zipFile.getEntry(filename)
      if (entry == null) return null
      var in: InputStream = zipFile.getInputStream(entry)
      var len: Int = entry.getSize.toInt
      var data: Array[Byte] = new Array[Byte](len)
      var success = 0
      var offset = 0
      while (success < len) {
        len -= success
        offset += success
        success = in.read(data, offset, len)
        if (success == -1) {
          val args: Array[AnyRef] = Array(clazzname, zipFile.getName)
          error(jEdit.getProperty("jar.error.zip", args))
          throw new ClassNotFoundException(clazzname)
        }
      }
      cls = defineClass(clazzname, data, 0, data.length)
      if (resolveIt) resolveClass(cls)
      cls
    }
    catch {
      case io: IOException => {
        error("I/O error:")
        io.printStackTrace
        throw new ClassNotFoundException(clazzname)
      }
    }
  }

  override def getResource(name: String): URL = {
    try {
      return new URL(getResourceAsPath(name))
    }
    catch {
      case mu: MalformedURLException => {
        return null
      }
    }
  }

  private def getResourceAsPath(name: String): String = {
    if (zipFile.getName.toLowerCase.endsWith(".jar")) "jar:" + zipFile.getName + "#" + name
    else if (zipFile.getName.toLowerCase.endsWith(".zip")) "zip:" + zipFile.getName + "#" + name
    else ""
  }

  override def getResourceAsStream(name: String): InputStream = {
    try {
      var entry: ZipEntry = zipFile.getEntry(name)
      if (entry == null) ClassLoader.getSystemResourceAsStream(name)
      else zipFile.getInputStream(entry)
    }
    catch {
      case io: IOException => {
        error("I/O error:")
        io.printStackTrace
        null
      }
    }
  }
}