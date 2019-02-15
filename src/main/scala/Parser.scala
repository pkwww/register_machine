import java.util

class Parser(tokenList: util.ArrayList[Token]) {
  val _tokenList = tokenList
  var _next = 0

  private def parseRegSpec(): util.ArrayList[Int] = {
    if (expect(REGISTERS)) {
      val regNum = parseNum()
      val registers = new util.ArrayList[Int]()
      var i = 0
      while (_tokenList.get(_next).tokenType != NL && i < regNum) {
        registers.add(parseNum())
        i += 1
      }
      if (i >= regNum) {
        System.out.println("Warning: ignoring extra init value")
      }
      consumeOverNewLine()
      registers
    } else {
      throw new Exception("unexpected token, expected \"registers\", get: " + _tokenList.get(_next).token)
    }
  }

  private def parseInst(): Unit = {

  }

  private def expect(tokenType: TokenType): Boolean = {
    val isExpected = _tokenList.get(_next).tokenType.equals(tokenType)
    _next += 1
    isExpected
  }

  private def isRegistersToken(token: Token): Boolean = {
    token.tokenType == REGISTERS
  }

  private def isNum(token: Token): Boolean = {
    token.tokenType == NUMBER
  }

  private def isRNum(token: Token): Boolean = {
    token.tokenType == RNUM
  }

  private def isNewLine(token: Token): Boolean = {
    token.tokenType == NL
  }

  private def parseNum(): Int = {
    val token = _tokenList.get(_next)
    if (isNum(token)) {
      _next += 1
      token.token.toInt
    } else {
      throw new Exception("Unexpected token, expect Number, get: " + token.token)
    }
  }

  private def parseRNum(): Int = {
    val token = _tokenList.get(_next)
    if (isRNum(token)) {
      _next += 1
      token.token.substring(1).toInt
    } else {
      throw new Exception("Unexpected token, expect r<Number>, get: " + token.token)
    }
  }

  private def consumeNewLine(): Unit = {
    val tokensLen = _tokenList.size()
  }

  private def consumeOverNewLine(): Unit = {
    val tokensLen = _tokenList.size()
    while (_next < tokensLen && !isNewLine(_tokenList.get(_next))) {
      _next += 1
    }
    while (_next < tokensLen && isNewLine(_tokenList.get(_next))) {
      _next += 1
    }
  }
}
