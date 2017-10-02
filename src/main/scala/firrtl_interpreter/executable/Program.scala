// See LICENSE for license details.

package firrtl_interpreter.executable

case class Program(symbolTable: SymbolTable, dataStore: DataStore, scheduler: Scheduler) {
  def header: String = {
    symbolTable.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
  }

  def dataInColumns: String = {
    symbolTable.keys.toArray.sorted.map { name =>
      val value = dataStore(symbolTable(name))
      f"$value%10.10s" }.mkString("")
  }
}
