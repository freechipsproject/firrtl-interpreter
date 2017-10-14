// See LICENSE for license details.

package firrtl_interpreter.executable

case class Program(
                    symbolTable: SymbolTable,
                    dataStore: DataStore,
                    scheduler: Scheduler
                  ) {
  def header: String = {
    symbolTable.keys.toArray.sorted.map { name =>
      val s = s"$name:${symbolTable(name).index}"
      f"$s%10.10s"
    }.mkString("")
  }

  def dataInColumns: String = {
    ("-" * 100) + s"\n${dataStore.previousBufferIndex}  " +
    symbolTable.keys.toArray.sorted.map { name =>
      val value = dataStore.previousIntArray(symbolTable(name).index)
      f"$value%10.10s" }.mkString("") + s"\n${dataStore.currentBufferIndex}  " +
    symbolTable.keys.toArray.sorted.map { name =>
      val value = dataStore.currentIntArray(symbolTable(name).index)
      f"$value%10.10s" }.mkString("") + "\n" +
    ("-" * 100)
  }
}
