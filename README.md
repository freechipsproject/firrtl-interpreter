Firrtl-Interpreter
==================

The firrtl interpreter is a tool in the [UCB-BAR/chisel](https://github.com/ucb-bar) hardware synthesis toolbox. 
[Chisel3](https://github.com/ucb-bar/chisel3.git) is a high-level functional circuit generator.  It produces **Flexible Intermediate
Representation for RTL** or **FIRRTL*.  [Chisel3](https://github.com/ucb-bar/chisel3.git) is a toolkit for lowering FIRRTL to verilog.
This interpreter parse and execute the LoFirrtl subset of Firrtl.  The interpreter is useful for a initial debugging of Chisel circuits
and is also used for other forms of circuit analysis.  In combination with a scala debugger such as Eclipse or IntelliJ it can be 
a very power way of inspecting the run-time behavior of a circuit

## Using the interpreter
### Attach it to your project
Most Chisel development projects will have a an SBT file that describes dependencies.  To access the firrtl-interpreter in your project
add a dependency on
```
"edu.berkeley.cs" %% "firrtl-interpreter" % "0.1-SNAPSHOT"
```
There are a number of different ways to specify this dependency in the build.sbt file.  If you have based your circuit on the 
[Chisel-template](https://github.com/ucb-bar/chisel-template.git) the addition should look like
```scala
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "edu.berkeley.cs" %% "chisel-iotesters" % "1.0",
  "edu.berkeley.cs" %% "firrtl-interpreter" % "0.1-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.scalacheck" %% "scalacheck" % "1.12.4")
```
for other idioms consult **sbt** documentation

### Use the tester metaphor
The easiest way to invoke the interpreter is through a test based harness.  The [[InterpretiveTester]] is very similar to the chisel
ClassicTester


### Did it work?

### Using the interpreter with a debugger