namespace SqlInterp

module Token =
    type Span =
        { Start: int
          End: int }

        member this.toDebugString() =
            sprintf "{ Start: %d, End: %d }" this.Start this.End

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

        member this.toDebugString() =
            match this with
            | Identifier str -> $"Identifier: {str}"
            | QuotedIdentifier str -> $"QuotedIdentifier: {str}"
            | StringLiteral str -> $"StringLiteral: {str}"
            | NumberLiteral str -> $"NumberLiteral: {str}"
            | LocalVariable str -> $"LocalVariable: {str}"
            | Comment str -> $"Comment: {str}"
            | Comma -> "Comma"
            | LeftParen -> "LeftParen"
            | RightParen -> "RightParen"
            | Equal -> "Equal"
            | BangEqual -> "BangEqual"
            | LessThanGreaterThan -> "LessThanGreaterThan"
            | LessThan -> "LessThan"
            | LessThanEqual -> "LessThanEqual"
            | GreaterThan -> "GreaterThan"
            | GreaterThanEqual -> "GreaterThanEqual"
            | Plus -> "Plus"
            | Minus -> "Minus"
            | ForwardSlash -> "ForwardSlash"
            | Asterisk -> "Asterisk"
            | PercentSign -> "PercentSign"
            | Period -> "Period"
            | SemiColon -> "SemiColon"
            | Eof -> "Eof"
            | Illegal -> "Illegal"
            | By -> "By"
            | Distinct -> "Distinct"
            | From -> "From"
            | Order -> "Order"
            | Percent -> "Percent"
            | Select -> "Select"
            | Ties -> "Ties"
            | Top -> "Top"
            | With -> "With"

    let stringToKeyword str =
        match str with
        | "by" -> Some By
        | "distinct" -> Some Distinct
        | "from" -> Some From
        | "order" -> Some Order
        | "percent" -> Some Percent
        | "select" -> Some Select
        | "ties" -> Some Ties
        | "top" -> Some Top
        | "with" -> Some With
        | _ -> None

    type Token =
        { Type: TokenType
          Span: Span }

        member this.toDebugString() =
            sprintf "{ TokenType: \"%s\", Span: %s }" (this.Type.toDebugString ()) (this.Span.toDebugString ())
