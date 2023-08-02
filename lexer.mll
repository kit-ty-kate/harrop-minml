{ exception Error }

let alpha = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let lident = (['a'-'z'] alpha*)
let int = ['0'-'9']+
let blank = [' ' '\t']

rule main = parse
  | blank { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | '(' { Parser.LParen }
  | ')' { Parser.RParen }
  | "if" { Parser.If }
  | "then" { Parser.Then }
  | "else" { Parser.Else }
  | "<=" { Parser.Leq }
  | '+' { Parser.Add }
  | '-' { Parser.Sub }
  | "let" { Parser.Let }
  | "rec" { Parser.Rec }
  | '=' { Parser.Eq }
  | "do" { Parser.Do }
  | int as n { Parser.INT n }
  | lident as id { Parser.LIDENT id }
  | eof { Parser.EOF }
  | _ { raise Error }
