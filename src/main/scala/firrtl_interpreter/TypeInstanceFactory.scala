// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ir._

/**
  * Created by chick on 4/21/16.
  */

object TypeInstanceFactory {
  def apply(typ: Type): Concrete = {
    typ match {
      case u: UIntType => Concrete.poisonedUInt(widthToInt(u.width))
      case s: SIntType => Concrete.poisonedSInt(widthToInt(s.width))
      case ClockType   => ConcreteUInt(0, 1)
      case _ => throw new InterpreterException(s"Unsupported LoFIRRTL type for interpreter $typ")
    }
  }
  def apply(typ: Type, initialValue: BigInt): Concrete = {
    typ match {
      case u: UIntType => ConcreteUInt(initialValue, widthToInt(u.width))
      case s: SIntType => ConcreteSInt(initialValue, widthToInt(s.width))
      case ClockType   => ConcreteUInt(if(initialValue > 0) 1 else 0, 1)
      case _ => throw new InterpreterException(s"Unsupported LoFIRRTL type for interpreter $typ")
    }
  }
  def apply(template: Concrete, value: BigInt): Concrete = {
    template match {
      case ConcreteUInt(_, width, p) => ConcreteUInt(value, width, p)
      case ConcreteSInt(_, width, p) => ConcreteSInt(value, width, p)
    }
  }
}
