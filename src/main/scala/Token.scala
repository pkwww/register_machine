sealed class Token(val token: String, val tokenType: TokenType) {
  override def toString: String = {
    token + " " + tokenType.toString
  }
}

final case class SpaceToken(override val token: String) extends Token(token, SP)
final case class NewLineToken(override val token: String) extends Token(token, NL)
final case class NumberToken(override val token: String) extends Token(token, NUMBER)
final case class RegistersToken(override val token: String) extends Token(token, REGISTERS)
final case class IncToken(override val token: String) extends Token(token, INC)
final case class DecjzToken(override val token: String) extends Token(token, DECJZ)
final case class RnumToken(override val token: String) extends Token(token, RNUM)

sealed trait TokenType

case object SP extends TokenType
case object NL extends TokenType
case object NUMBER extends TokenType
case object REGISTERS extends TokenType
case object INC extends TokenType
case object DECJZ extends TokenType
case object RNUM extends TokenType

