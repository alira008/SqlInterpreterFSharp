namespace SqlInterp

module Token =
    type Span =
        { Start: int
          End: int }

        member toDebugString: unit -> string

    type TokenType =
        | Identifier of string
        | QuotedIdentifier of string
        | StringLiteral of string
        | NumberLiteral of string
        | LocalVariable of string
        | Comment of string
        | Comma
        | LeftParen
        | RightParen
        | Equal
        | BangEqual
        | LessThanGreaterThan
        | LessThan
        | LessThanEqual
        | GreaterThan
        | GreaterThanEqual
        | Plus
        | Minus
        | ForwardSlash
        | Asterisk
        | PercentSign
        | Period
        | SemiColon
        | Eof
        | Illegal
        // keywords
        | By
        | Distinct
        | From
        | Order
        | Percent
        | Select
        | Ties
        | Top
        | With
        | Where

        member toDebugString: unit -> string

    val stringToKeyword: string -> Option<TokenType>

    type Token =
        { Type: TokenType
          Span: Span }

        member toDebugString: unit -> string
