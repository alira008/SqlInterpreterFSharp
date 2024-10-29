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

    let isNotClosingSquareBracket char = char <> ']'
    let isNotClosingStringLiteral char = char <> '\''
    let isNotNewLine char = char <> '\n'
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

    let readQuotedIdentifier lexer =
        let lexer = readChar lexer
        let mutable lexer, str = readWhile lexer isNotClosingSquareBracket

        if str.Length = 0 then
            lexer, Error "quoted identifier cannot have empty string"
        else if lexer.Ch.IsSome && lexer.Ch.Value = ']' then
            lexer <- readChar lexer
            lexer, Ok str
        else
            lexer, Error "quoted identifier closing square bracket not found"

    let readStringLiteral lexer =
        let lexer = readChar lexer
        let mutable lexer, str = readWhile lexer isNotClosingStringLiteral

        if str.Length = 0 then
            lexer, Error "string literal cannot have empty string"
        else if lexer.Ch.IsSome && lexer.Ch.Value = '\'' then
            lexer <- readChar lexer
            lexer, Ok(str)
        else
            lexer, Error "string literal closing square apostrophe not found"

    let readComment lexer =
        // skip -- characters
        let mutable lexer = readChar lexer
        lexer <- readChar lexer
        lexer <- skipWhitespace lexer
        let lexer, str = readWhile lexer isNotNewLine

        lexer, str

    let readNumberLiteral lexer =
        let mutable lexer, str = readWhile lexer isNumber
        let nextCh = peekChar lexer

        if nextCh.IsSome && nextCh.Value = '.' then
            lexer <- readChar lexer
            let lexer, str = readWhile lexer isNumber
            lexer, str
        else
            lexer, str

    let convertStringToNumberLiteral (lexer, str) = lexer, Ok(Token.NumberLiteral str)
    let convertTokenTypeToResult (lexer, tokenType) = lexer, Ok tokenType
    let convertStringToComment (lexer, str) = lexer, Ok (Token.Comment str)
    let convertStringToStringLiteral (lexer,  result) =
        match result with
        | Error v -> lexer, Error v
        | Ok str -> lexer, Ok(Token.StringLiteral str)

    let convertStringToQuotedIdentifier (lexer, result) =
        match result with
        | Error v -> lexer, Error v
        | Ok str -> lexer, Ok(Token.QuotedIdentifier str)


    let nextToken lexer =
        let lexer = lexer |> skipWhitespace
        let start = lexer.CurrentPosition
        let advance tokenType = lexer |> readChar, tokenType
        let advanceOk tokenType = lexer |> readChar, Ok tokenType

        let lexer, tokenType =
            match lexer.Ch, peekChar lexer with
            | None, None -> lexer, Ok Token.Eof
            | Some ',', _ -> advanceOk Token.Comma
            | Some '(', _ -> advanceOk Token.LeftParen
            | Some ')', _ -> advanceOk Token.RightParen
            | Some '=', _ -> advanceOk Token.Equal
            | Some '!', Some '=' -> advanceOk Token.BangEqual
            | Some '<', Some '>' -> advanceOk Token.LessThanGreaterThan
            | Some '<', Some '=' -> advanceOk Token.BangEqual
            | Some '<', _ -> advanceOk Token.LessThan
            | Some '>', Some '=' -> advanceOk Token.GreaterThanEqual
            | Some '>', _ -> advanceOk Token.GreaterThan
            | Some '+', _ -> advanceOk Token.Plus
            | Some '-', Some '-' -> lexer |> readComment |> convertStringToComment
            | Some '-', _ -> advanceOk Token.Minus
            | Some '/', _ -> advanceOk Token.ForwardSlash
            | Some '*', _ -> advanceOk Token.Asterisk
            | Some '%', _ -> advanceOk Token.PercentSign
            | Some '.', _ -> advanceOk Token.Period
            | Some ';', _ -> advanceOk Token.SemiColon
            | Some '\'', _ -> lexer |> readStringLiteral |> convertStringToStringLiteral
            | Some '[', _ -> lexer |> readQuotedIdentifier |> convertStringToQuotedIdentifier
            | Some '_', Some nextCh when nextCh |> isLetter -> lexer |> readIdentifier |> convertTokenTypeToResult
            | Some ch, _ when ch |> isLetter -> lexer |> readIdentifier |> convertTokenTypeToResult
            | Some ch, _ when ch |> isNumber -> lexer |> readNumberLiteral |> convertStringToNumberLiteral
            | _ -> advance (Error "illegal character received")

        match tokenType with
        | Error v -> Error v
        | Ok tt ->
            Ok(
                lexer,
                { Token.Type = tt
                  Token.Span =
                    { Start = start
                      End = lexer.CurrentPosition-1 } }
            )

    let tokenizeInput lexer =
        let rec loop lexer =
            seq {
                match lexer |> nextToken with
                | Ok(_, token) when token.Type = Token.Eof -> yield token
                | Ok(lexer, token) ->
                    yield token
                    yield! loop lexer
                | Error _ -> yield! loop lexer
            }

        loop lexer


    let tokenize input = input |> createLexer |> tokenizeInput
