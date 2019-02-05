object Main {
  def main(args: Array[String]): Unit = {
    val input = "registers 3 10 5\ndecjz r1 3\n"
    val lexer = new Lexer(input)
    val tokens = lexer.tokenizer()
    tokens.forEach(println)
  }
}
