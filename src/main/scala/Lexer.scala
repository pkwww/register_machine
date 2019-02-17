import java.util

import scala.util.matching.Regex
import java.util.ArrayList

class Lexer(input: String) {
  private val letterRegex: Regex = "([a-zA-Z])".r
  private val numRegex: Regex = "([0-9])".r
  private val spRegex: Regex = "(\t| )".r
  private val nlRegex: Regex = "(\r|\n)".r
  private val commentRegex: Regex = "(#)".r
  private val _input: String = input
  private val _size: Int = _input.length
  private var _i: Int = 0

  // TODO: Token become case class
  // TODO: ArrayList to scala collection
  def tokenizer(): util.ArrayList[Token] = {
    val tokens = new util.ArrayList[Token]()
    while (_i < _size) {
      val c = _input(_i)
      c match {
        case letterRegex(_*) => tokens.add(letterToken())
        case numRegex(_*) => tokens.add(numberToken())
        case spRegex(_*) => consumeSpace()
        case nlRegex(_*) => tokens.add(newLineToken())
        case commentRegex(_*) => consumeLine()
        case _ => throw new Exception("unexpected char: " + c)
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
      case "registers" => RegistersToken(token)
      case "inc" => IncToken(token)
      case "decjz" => DecjzToken(token)
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
    RnumToken(tokenR + regNum)
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
    NumberToken(token)
  }

  private def newLineToken(): Token = {
    val token = _input.substring(_i, _i + 1)
    _i += 1
    NewLineToken(token)
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
    matchRegex(letterRegex, c)
  }

  private def isNum(c: Char):Boolean = {
    matchRegex(numRegex, c)
  }

//  private def isSpace(c: Char):Boolean = {
//    matchRegex(spRegex, c)
//  }
//
//  private def isNewLine(c: Char):Boolean = {
//    matchRegex(nlRegex, c)
//  }
//
//  private def isComment(c: Char):Boolean = {
//    c == '#'
//  }

  private def matchRegex(regex: Regex, c: Char):Boolean = {
    val s = new Array[Char](1)
    s(0) = c
    regex.findFirstMatchIn(s) match {
      case Some(_) => true
      case None => false
    }
  }
}
