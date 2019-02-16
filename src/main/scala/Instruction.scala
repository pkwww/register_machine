sealed trait Instruction

final case class IncInst(val regIndex: Int) extends Instruction

final case class DeczjInst(val regIndex: Int, val instIndex: Int) extends Instruction
