
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | VIRG
  | VEZES
  | TRUE
  | STR
  | SETA
  | RETURN
  | REAL
  | RANGE
  | PV
  | PRINT
  | PONTO
  | PASS
  | OU
  | NOVALINHA
  | NOT
  | MODULO
  | MENOS
  | MENORIGUAL
  | MENOR
  | MAIS
  | MAIORIGUAL
  | MAIOR
  | Linha of (int * int * token list)
  | LITSTRING of (string)
  | LITINT of (int)
  | LITFLOAT of (float)
  | LIST
  | LEN
  | IS
  | INT
  | INPUT
  | INDENTA
  | INCREMENTA
  | INCR
  | IN
  | IMPORT
  | IGUALDADE
  | IF
  | ID of (string)
  | FROM
  | FPAR
  | FOR
  | FLOAT
  | FECHACOLCHETES
  | FECHACHAVES
  | FALSE
  | EOF
  | ELSE
  | ELIF
  | E
  | DPONTOS
  | DOUBLE
  | DIVIDIDO
  | DIFERENTE
  | DEF
  | DEDENTA
  | DECREMENTA
  | DECR
  | CHAR
  | BREAK
  | BOOL
  | ATRIBMULT
  | ATRIBMENOS
  | ATRIBMAIS
  | ATRIBDIV
  | ATRIB
  | ASPASSIMPLES
  | ASPASDUPLAS
  | APAR
  | ABRECOLCHETES
  | ABRECHAVES

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)
