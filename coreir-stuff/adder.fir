;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-10-09 23:50:54.247, builtAtMillis: 1507593054247
circuit Adder : 
  module Adder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in0 : UInt<8>, flip in1 : UInt<8>, out : UInt<8>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = add(io.in0, io.in1) @[Adder.scala 17:20]
    node _T_6 = tail(_T_5, 1) @[Adder.scala 17:20]
    io.out <= _T_6 @[Adder.scala 17:10]
    
