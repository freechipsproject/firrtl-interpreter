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
    val keys = symbolTable.keys.toArray.sorted
    ("-" * 100) + f"\n${dataStore.previousBufferIndex}%2s  " +
    keys.map { name =>
      val symbol = symbolTable(name)
      val value = symbol.normalize(dataStore.earlierValue(symbolTable(name), 1))
      f"$value%10.10s" }.mkString("") + f"\n${dataStore.currentBufferIndex}%2s  " +
    keys.map { name =>
      val symbol = symbolTable(name)
      val value = symbol.normalize(dataStore(symbolTable(name)))
      f"$value%10.10s" }.mkString("") + "\n" +
    ("-" * 100)
  }
}
