import java.util

class Machine(val registers: util.ArrayList[Int], val instructions: util.ArrayList[Instruction]) {
  private var _line: Int = 0
  private val progLen = instructions.size()
  private val registersLen = registers.size()

  def eval(): Unit = {
    while (_line < progLen) {
      val instruction = instructions.get(_line)
      instruction match {
        case IncInst(regIndex) => doInc(regIndex)
        case DeczjInst(regIndex, instIndex) => doDeczj(regIndex, instIndex)
      }
    }
  }

  private def doInc(regIndex: Int): Unit = {
    if (regIndex < registersLen) {
      registers.set(regIndex, registers.get(regIndex) + 1)
      _line += 1
    } else {
      throw new Exception("Register Index overflow")
    }
  }

  private def doDeczj(regIndex: Int, instIndex: Int): Unit = {
    if (regIndex < registersLen) {
      if (registers.get(regIndex) != 0) {
        registers.set(regIndex, registers.get(regIndex) - 1)
        _line += 1
      } else {
        _line = instIndex
      }
    } else {
      throw new Exception("Register Index overflow")
    }
  }
}
