/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package firrtl_interpreter

import java.io.File

case class ScriptFactory(parent: FirrtlRepl) {
  val console = parent.console
  var lastFileOption: Option[String] = None

  def apply(fileName: String): Option[Script] = {
    val file = new File(fileName)
    if(! file.exists()) {
      throw new Exception(s"file $fileName does not exist")
    }
    val scriptLines = io.Source.fromFile(file).mkString.split("\n")
    val script = new Script(fileName, scriptLines)
    Some(script)
  }
}

class Script(val fileName: String, val lines: Array[String]) {
  var currentLine = -1
  var linesLeftToRun = 0

  def getNextLineOption: Option[String] = {
    if(hasNext) {
      currentLine += 1
      linesLeftToRun -= 1
      val nextLine = lines(currentLine)
      Some(nextLine)
    }
    else {
      None
    }
  }

  def hasNext: Boolean = {
    currentLine < lines.length-1 && linesLeftToRun > 0
  }

  def length: Int = lines.length

  def setLinesToRun(n: Int): Unit = {
    linesLeftToRun = n.min((lines.length - currentLine) + 1)
  }

  def setSkipLines(n: Int): Unit = {
    for(i <- 0 until n) {
      getNextLineOption
    }
  }

  def setLine(n: Int): Unit = {
    reset()
    currentLine = n - 1
  }

  def runRemaining(): Unit = {
    linesLeftToRun = (lines.length - currentLine) + 1
  }

  def atEnd: Boolean = {
    currentLine == lines.length - 1
  }

  def reset(): Unit = {
    currentLine = -1
    linesLeftToRun = 0
  }

  override def toString: String = {
    lines.zipWithIndex.map { case (line, index) =>
      f"$index%3d" +
        (if(index == currentLine) "* " else "  " ) +
        line
    }.mkString("\n")
  }
}
