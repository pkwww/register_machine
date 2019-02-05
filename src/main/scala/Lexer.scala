import java.util

import scala.util.matching.Regex
import java.util.ArrayList

class Lexer(input: String) {
  private val LETTER_REGEX: Regex = "[a-zA-Z]".r
  private val NUM_REGEX: Regex = "[0-9]".r
  private val SP_REGEX: Regex = "(\t| )".r
  private val NL_REGEX: Regex = "(\r|\n)".r
  private val _input: String = input
  private val _size: Int = _input.size
  private var _i: Int = 0

  def tokenizer(): ArrayList[Token] = {
    val tokens = new util.ArrayList[Token]()
    while (_i < _size) {
      val c = _input(_i)
      if (isLetter(c)) {
        tokens.add(letterToken())
      } else if (isNum(c)) {
        tokens.add(numberToken())
      } else if (isSpace(c)) {
        consumeSpace()
      } else if (isNewLine(c)) {
        tokens.add(newLineToken())
      } else if (isComment(c)) {
        consumeLine()
      } else {
        throw new Exception("unexpected char: " + c)
      }
    }
    tokens
  }

  private def letterToken(): Token = {
    val builder = new StringBuilder()
    var c = _input(_i)
    while (isLetter(c)) {
      builder.append(c)
      _i += 1
      c = _input(_i)
    }
    val token = builder.toString()
    token match {
      case "registers" => new Token(token, REGISTERS)
      case "inc" => new Token(token, INC)
      case "decjz" => new Token(token, DECJZ)
      case "r" => tokenRN(token)
      case _ => throw new Exception("unexpected token: " + token)
    }
  }

  private def tokenRN(tokenR: String): Token = {
    val builder = new StringBuilder()
    var c = _input(_i)
    if (!isNum(c)) {
      throw new Exception("missing register number!")
    }
    while (isNum(c)) {
      builder.append(c)
      _i += 1
      c = _input(_i)
    }
    val regNum = builder.toString()
    new Token(tokenR + regNum, RNUM)
  }

  private def numberToken(): Token = {
    val builder = new StringBuilder()
    var c = _input(_i)
    while (isNum(c)) {
      builder.append(c)
      _i += 1
      c = _input(_i)
    }
    val token = builder.toString()
    new Token(token, NUMBER)
  }

  private def newLineToken(): Token = {
    val token = _input.substring(_i, _i + 1)
    _i += 1
    new Token(token, NL)
  }

  private def consumeSpace(): Unit = {
    while (_input(_i) == ' ') {
      _i += 1
    }
  }

  private def consumeLine(): Unit = {
    while (_input(_i) != '\n') {
      _i += 1
    }
    _i += 1
  }

  private def isLetter(c: Char):Boolean = {
    matchRegex(LETTER_REGEX, c)
  }

  private def isNum(c: Char):Boolean = {
    matchRegex(NUM_REGEX, c)
  }

  private def isSpace(c: Char):Boolean = {
    matchRegex(SP_REGEX, c)
  }

  private def isNewLine(c: Char):Boolean = {
    matchRegex(NL_REGEX, c)
  }

  private def isComment(c: Char):Boolean = {
    c == '#'
  }

  private def matchRegex(regex: Regex, c: Char):Boolean = {
    val s = new Array[Char](1)
    s(0) = c
    regex.findFirstMatchIn(s) match {
      case Some(_) => true
      case None => false
    }
  }
}
