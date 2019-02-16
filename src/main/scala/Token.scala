class Token(val token: String, val tokenType: TokenType) {
  override def toString: String = {
    token + " " + tokenType.toString()
  }
}

sealed trait TokenType

case object EOF extends TokenType
case object SP extends TokenType
case object NL extends TokenType
case object NUMBER extends TokenType
case object REGISTERS extends TokenType
case object INC extends TokenType
case object DECJZ extends TokenType
case object RNUM extends TokenType

