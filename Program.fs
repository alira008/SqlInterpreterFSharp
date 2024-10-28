open SqlInterp.Token
open SqlInterp.Lexer

let input = "select * from polygonmarketquotes where yes > 4;"

let tokens = tokenize input

tokens
|> Seq.iter (fun (token: Token) -> printfn "%s" (token.toDebugString ()))
