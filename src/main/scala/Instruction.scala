sealed trait Instruction

final case class IncInst(regIndex: Int) extends Instruction

final case class DeczjInst(regIndex: Int, instIndex: Int) extends Instruction
