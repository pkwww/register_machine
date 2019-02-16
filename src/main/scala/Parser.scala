import java.util

class Parser(tokenList: util.ArrayList[Token]) {
  var _next = 0

  def parse(): Machine = {
    new Machine(parseRegSpec(), parseProg())
  }

  private def parseRegSpec(): util.ArrayList[Int] = {
    if (expect(REGISTERS)) {
      val regNum = parseNum()
      val registers = new util.ArrayList[Int]()
      var i = 0
      while (tokenList.get(_next).tokenType != NL && i < regNum) {
        registers.add(parseNum())
        i += 1
      }
      while (i < regNum) {
        registers.add(0)
      }
      if (i >= regNum) {
        System.out.println("Warning: ignoring extra init value")
      }
      consumeOverNewLine()
      registers
    } else {
      _next -= 1
      throw new Exception("unexpected token, expected \"registers\", get: " + tokenList.get(_next).token)
    }
  }

  private def parseProg(): util.ArrayList[Instruction] = {
    val len = tokenList.size()
    var prog = new util.ArrayList[Instruction]()
    while (_next < len) {
      prog.add(parseInst())
      consumeOverNewLine()
    }
    prog
  }

  private def parseInst(): Instruction = {
    val instToken = tokenList.get(_next)
    if (isInc(instToken)) {
      _next += 1
      val regIndex = parseNum()
      new IncInst(regIndex)
    } else if (isDeczj(instToken)) {
      _next += 1
      val regIndex = parseNum()
      val instIndex = parseNum()
      new DeczjInst(regIndex, instIndex)
    } else {
      throw new Exception("unexpected token, expected \"inc\" or \"deczj\", get: " + instToken.token)
    }
  }

  private def expect(tokenType: TokenType): Boolean = {
    val isExpected = tokenList.get(_next).tokenType.equals(tokenType)
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

  private def isInc(token: Token): Boolean = {
    token.tokenType == INC
  }

  private def isDeczj(token: Token): Boolean = {
    token.tokenType == DECJZ
  }

  private def parseNum(): Int = {
    val token = tokenList.get(_next)
    if (isNum(token)) {
      _next += 1
      token.token.toInt
    } else {
      throw new Exception("Unexpected token, expect Number, get: " + token.token)
    }
  }

  private def parseRNum(): Int = {
    val token = tokenList.get(_next)
    if (isRNum(token)) {
      _next += 1
      token.token.substring(1).toInt
    } else {
      throw new Exception("Unexpected token, expect r<Number>, get: " + token.token)
    }
  }

  private def consumeNewLine(): Unit = {
    val tokensLen = tokenList.size()
  }

  private def consumeOverNewLine(): Unit = {
    val tokensLen = tokenList.size()
    while (_next < tokensLen && !isNewLine(tokenList.get(_next))) {
      _next += 1
    }
    while (_next < tokensLen && isNewLine(tokenList.get(_next))) {
      _next += 1
    }
  }
}
