namespace SqlInterp

module Lexer =
    type Lexer =
        { Input: string
          Ch: option<char>
          CurrentPosition: int
          ReadPosition: int }

    val tokenize: input: string -> Token.Token seq
