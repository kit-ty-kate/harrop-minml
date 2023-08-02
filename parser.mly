%token If Then Else
%token Leq
%token Add Sub
%token Let Rec
%token Eq
%token Do
%token LParen RParen
%token <string> INT
%token <string> LIDENT
%token EOF

%start prog
%type <Ast.prog> prog

%%

expr:
  | If p = expr Then t = expr Else f = expr { If (p, t, f) }
  | e1 = expr Leq e2 = expr { BinOp (`Leq, e1, e2) }
  | e1 = expr Add e2 = expr { BinOp (`Add, e1, e2) }
  | e1 = expr Sub e2 = expr { BinOp (`Sub, e1, e2) }
  | f = expr x = expr { Apply (f, x) }
  | v = LIDENT { Var v }
  | n = INT { Int (int_of_string n) }
  | LParen e = expr RParen { e }

defn: Let Rec f = LIDENT x = LIDENT Eq body = expr { LetRec (f, x, body) }

prog: defns = defn* Do run = expr EOF { defns, run } 
