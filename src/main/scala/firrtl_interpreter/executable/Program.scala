// See LICENSE for license details.

package firrtl_interpreter.executable

case class Program(
                    symbolTable: SymbolTable,
                    dataStore: DataStore,
                    scheduler: Scheduler
                  ) {
  def header: String = {
    "Buf " +
      symbolTable.keys.toArray.sorted.map { name =>
      val s = name.takeRight(10)
      f"$s%10.10s"
    }.mkString("")
  }

  def dataInColumns: String = {
    ("-" * 100) + f"\n${dataStore.previousBufferIndex}%2s  " +
    symbolTable.keys.toArray.sorted.map { name =>
      val value = dataStore.earlierValue(symbolTable(name), 1)
      f"$value%10.10s" }.mkString("") + f"\n${dataStore.currentBufferIndex}%2s  " +
    symbolTable.keys.toArray.sorted.map { name =>
      val value = dataStore(symbolTable(name))
      f"$value%10.10s" }.mkString("") + "\n" +
    ("-" * 100)
  }
}
