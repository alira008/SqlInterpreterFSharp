namespace SqlInterp

module Lexer =
    type Lexer =
        { Input: string
          Ch: option<char>
          CurrentPosition: int
          ReadPosition: int }

    let readChar lexer =
        let newChar =
            if lexer.ReadPosition >= lexer.Input.Length then
                None
            else
                Some lexer.Input[lexer.ReadPosition]

        { lexer with
            CurrentPosition = lexer.ReadPosition
            ReadPosition = lexer.ReadPosition + 1
            Ch = newChar }

    let rec skipWhitespace lexer =
        match lexer.Ch with
        | Some ch when ch = ' ' || ch = '\r' || ch = '\n' || ch = '\t' -> readChar lexer |> skipWhitespace
        | _ -> lexer

    // Helper functions to identify character types
    let isLetter char =
        (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char = '_')

    let isNumber char = char >= '0' && char <= '9'

    let createLexer input =
        let lexer =
            { Input = input
              Ch = None
              CurrentPosition = 0
              ReadPosition = 0 }

        lexer |> readChar

    let peekChar lexer =
        if lexer.ReadPosition >= lexer.Input.Length then
            None
        else
            Some lexer.Input[lexer.ReadPosition]


    let readWhile lexer predicate =
        let start = lexer.CurrentPosition

        let rec read lexer =
            if lexer.Ch.IsSome && lexer.Ch.Value |> predicate then
                readChar lexer |> read
            else
                lexer

        let lexer = read lexer
        lexer, lexer.Input.Substring(start, lexer.CurrentPosition - start)

    let readIdentifier lexer =
        let lexer, str = readWhile lexer isLetter

        let possibleKeyword = str |> Token.stringToKeyword

        match possibleKeyword with
        | None -> lexer, Token.Identifier str
        | Some kw -> lexer, kw


    let nextToken lexer =
        let lexer = lexer |> skipWhitespace
        let start = lexer.CurrentPosition
        let advance tokenType = lexer |> readChar, tokenType

        let lexer, tokenType =
            match lexer.Ch, peekChar lexer with
            | None, None -> lexer, Token.Eof
            | Some ',', _ -> advance Token.Comma
            | Some '(', _ -> advance Token.LeftParen
            | Some ')', _ -> advance Token.RightParen
            | Some '=', _ -> advance Token.Equal
            | Some '!', Some '=' -> advance Token.BangEqual
            | Some '<', Some '>' -> advance Token.LessThanGreaterThan
            | Some '<', Some '=' -> advance Token.BangEqual
            | Some '<', _ -> advance Token.LessThan
            | Some '>', Some '=' -> advance Token.GreaterThanEqual
            | Some '>', _ -> advance Token.GreaterThan
            | Some '+', _ -> advance Token.Plus
            | Some '-', _ -> advance Token.Minus
            | Some '/', _ -> advance Token.ForwardSlash
            | Some '*', _ -> advance Token.Asterisk
            | Some '%', _ -> advance Token.PercentSign
            | Some '.', _ -> advance Token.Period
            | Some ';', _ -> advance Token.SemiColon
            | Some '_', Some nextCh when nextCh |> isLetter -> lexer |> readIdentifier
            | Some ch, _ when ch |> isLetter -> lexer |> readIdentifier
            | _ -> advance Token.Illegal

        lexer,
        { Token.Type = tokenType
          Token.Span =
            { Start = start
              End = lexer.CurrentPosition } }

    let tokenizeInput lexer =
        let rec loop lexer =
            seq {
                match lexer |> nextToken with
                | _, token when token.Type = Token.Eof -> yield token
                | lexer, token ->
                    yield token
                    yield! loop lexer
            }

        loop lexer


    let tokenize input = input |> createLexer |> tokenizeInput
