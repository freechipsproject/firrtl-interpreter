// See LICENSE for license details.

package firrtl_interpreter.timedomain

trait Event {
  def run(): Unit
}
