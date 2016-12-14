// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ir._

/**
  * Created by chick on 4/21/16.
  */

object TypeInstanceFactory {
  /**
    * makes a ConcreteSInt with value, width and poison
    * handles unsigned positive values as negatives
    * @param value new value for SInt
    * @param width width of new SInt
    * @param poisoned whether to poison the new SInt
    * @return
    */
  private def makeSInt(value: BigInt, width: Int, poisoned: Boolean = false): ConcreteSInt = {
    if(value > 0) {
      val uInt = ConcreteUInt(value, width, poisoned = poisoned)
      uInt.asSInt
    }
    else {
      ConcreteSInt(value, width, poisoned = poisoned)
    }
  }

  /**
    * Makes a poisoned Concrete based on the firrtl type
    * @param typ firrtl type
    * @return
    */
  def apply(typ: Type): Concrete = {
    typ match {
      case u: UIntType => Concrete.poisonedUInt(widthToInt(u.width))
      case s: SIntType => Concrete.poisonedSInt(widthToInt(s.width))
      case ClockType   => ConcreteUInt(0, 1)
      case _ => throw new InterpreterException(s"Unsupported LoFIRRTL type for interpreter $typ")
    }
  }

  /**
    * Make a new concrete value based on type and desired value
    * @param typ The type of the new Concrete
    * @param initialValue the value to give it
    * @param poisoned whether new concrete should be poisoned
    * @return
    */
  def apply(typ: Type, initialValue: BigInt, poisoned: Boolean = false): Concrete = {
    typ match {
      case u: UIntType => ConcreteUInt(initialValue, widthToInt(u.width), poisoned)
      case s: SIntType => makeSInt(initialValue, widthToInt(s.width), poisoned)
      case ClockType   => ConcreteClock(if(initialValue > 0) 1 else 0)
      case _ => throw new InterpreterException(s"Unsupported LoFIRRTL type for interpreter $typ")
    }
  }

  /**
    * Make a random valued concrete based on the properties of template, but using parameter poisoned
    * It's more convenient to use the Concrete#random functions because they figure out the proper
    * range for the random values generated
    * @param template The concrete value to use as the template
    * @param poisoned Sets poisoned
    * @return
    */
  def makeRandomSimilar(template: Concrete, poisoned: Boolean): Concrete = {
    template match {
      case u: ConcreteUInt => Concrete.randomUInt(u.width, poisoned)
      case s: ConcreteSInt => Concrete.randomSInt(s.width, poisoned)
      case s: ConcreteClock => Concrete.randomClock()
      case _ => throw new InterpreterException(s"Unsupported LoFIRRTL type for interpreter $template")
    }
  }
  /**
    * Make a concrete based on the properties of template, but using parameter poisoned
    * @param template The concrete value to use as the template
    * @param poisoned Sets poisoned
    * @return
    */
  def makeSimilar(template: Concrete, value: BigInt, poisoned: Boolean): Concrete = {
    template match {
      case ConcreteUInt(_, width, p) => ConcreteUInt(value, width, poisoned)
      case ConcreteSInt(_, width, p) => makeSInt(value, width, poisoned = poisoned)
    }
  }
}
