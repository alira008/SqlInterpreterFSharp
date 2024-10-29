open SqlInterp.Token
open SqlInterp.Lexer

let input = "select *, 'yes' [no] -- test comment \nfrom polygonmarketquotes where yes > 4;"

let tokens = tokenize input

tokens
|> Seq.iter (fun (token: Token) -> printfn "%s" (token.toDebugString ()))
