// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

class DependencyManager {
  val nameToDependency: mutable.HashMap[String, mutable.HashSet[String]] = {
    new mutable.HashMap[String, mutable.HashSet[String]] {
      override def default(key: String): mutable.HashSet[String] = {
        this (key) = new mutable.HashSet[String]
        this (key)
      }
    }
  }

  var numberOfNodes:      Long = 0L
  var numberOfMuxes:      Long = 0L
  var numberOfStatements: Long = 0L
}
