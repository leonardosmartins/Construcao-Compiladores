%{
	open Ast

%}
%token <float> LITFLOAT
%token <string> ID
%token <string> LITSTRING
%token <int> LITINT
%token IMPORT
%token DEF
%token ASPASSIMPLES
%token DPONTOS
%token PONTO
%token VIRG
%token MAIOR
%token MAIORIGUAL
%token MENOR
%token MENORIGUAL
%token DIFERENTE
%token IGUALDADE
%token MAIS
%token MENOS
%token DIVIDIDO
%token VEZES
%token MODULO
%token APAR
%token FPAR
%token ABRECOLCHETES
%token FECHACOLCHETES
%token ATRIB
%token IN
%token PRINT
%token INPUT
%token WHILE
%token FOR
%token IF
%token ELSE
%token RANGE
%token LEN
%token OU
%token E
%token NOT
%token RETURN
%token TRUE
%token FALSE
%token ASPASDUPLAS
%token INCREMENTA
%token DECREMENTA
%token SETA
%token EOF
%token ATRIBMAIS 
%token ATRIBMENOS 
%token ATRIBMULT 
%token ATRIBDIV 
%token ABRECHAVES 
%token FECHACHAVES 
%token PV
%token LIST
%token PASS
%token ELIF
%token VOID
%token STR
%token BREAK
%token IS
%token FROM
%token INT
%token FLOAT
%token REAL
%token DOUBLE
%token CHAR
%token INCR
%token DECR
%token BOOL
%token  <int * int * token list> Linha 
%token INDENTA
%token DEDENTA
%token NOVALINHA

%left OU
%left E
%left IGUALDADE DIFERENTE
%left MAIOR MAIORIGUAL MENOR MENORIGUAL
%left MAIS MENOS
%left VEZES DIVIDIDO MODULO

%start <Ast.prog> prog

%%
   
prog:
    | s=seq+ EOF 				{ Prog(s) }  
    | s=seq+ NOVALINHA EOF 				{ Prog(s) }         
    | NOVALINHA s=seq+ EOF 				{ Prog(s) } 
    | NOVALINHA s=seq+ NOVALINHA EOF 				{ Prog(s) }
    ;
     
seq:
	| e=expr 		{ Expressao(e) }
	| c=command 		{ Comando(c) }
	| f=func 		{ Funcao(f) }
	;

command:
	| l=LITINT 		{ LITINT(l) }
	| l=LITFLOAT 		{ LITFLOAT(l) }
	| l=LITSTRING 		{ LITSTRING(l) }
	| b=booleano 		{ BOOLEANO(b) }
	| i=ID	{ ID(i) }
	;	

parametro:
	| c=command tp=tiposPrimitivos 		{ Param(c,tp) }
	;
	
func:
	| DEF c=command APAR p=parametro* FPAR DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ DefFuncao(c,p,s) }
   	;

op:
	| MAIS 		{ Mais }
	| MENOS 		{ Menos }
	| DIVIDIDO 		{ Dividido }
	| VEZES 		{ Multiplicacao }
	| MODULO 		{ Modulo }
	;

operacao_op:
   | c1=command o=op c2=command 		{ Operacao_op(c1,o,c2) }
   ;

operacao:
	| op = operacao_op 		{ Operacao(op) }
	| op1=operacao_op o=op op2=operacao_op 		{ OperacaoOperacao(op1,o,op2) } 
	| op=operacao_op o=op c=command 		{ OperacaoComando(op,o,c) } 
	;  

comparador:
	| MENORIGUAL 		{ MenorIgual }
	| IGUALDADE 		{ Igualdade }
	| MAIORIGUAL 		{ MaiorIgual }
	| MAIOR 		{ Maior }
	| MENOR 		{ Menor }
	;

comparacao:
   | c1=command o=comparador c2=command 		{ Comparacao(c1,o,c2) }
   ;

logica:
	| E 		{ ELogico }
	| OU 		{ OULogico }
	;

verificacao:
	| c=comparacao 		{ Verificacao(c) }
	| c1=comparacao l=logica c2=comparacao 		{ VerificacaoDupla(c1,l,c2) }
	| APAR c1=comparacao l1=logica c2=comparacao FPAR l2=logica APAR c3=comparacao l3=logica c4=comparacao FPAR 		{ VerificacaoMultipla(c1,l1,c2,l2,c3,l3,c4) }
	;

expr:
   | o=operacao NOVALINHA		{ ExprOperacao(o) }
   | IMPORT c=command NOVALINHA		{ ExprImport(c) }
   | FROM c1=command IMPORT c2=command NOVALINHA		{ ExprFromImport(c1,c2) }
   | INPUT APAR FPAR NOVALINHA		{ ExprInput }
   | c=command ATRIB o=operacao NOVALINHA		{ ExprAtribCmdOp(c,o) }   
   | c1=command ATRIB c2=command NOVALINHA 		{ ExprAtribCmdCmd(c1,c2) } 
   | c=command ATRIB INT APAR INPUT APAR FPAR FPAR NOVALINHA 		{ ExprAtribCmdInput(c) }
   | c=command ATRIB INPUT APAR FPAR NOVALINHA 		{ ExprAtribCmdInput(c) } 
   | c1=command ATRIB c2=command APAR c3=command FPAR NOVALINHA		{ ExprAtribCmdCmdCmd(c1,c2,c3) }
   | c1=command ATRIB c2=command APAR FPAR NOVALINHA 		{ ExprAtribCmdCmd(c1,c2) }
   | PRINT APAR c=command FPAR NOVALINHA 		{ ExprPrint(c) }
   | PRINT APAR c1=command VIRG c2=command FPAR NOVALINHA 		{ ExprPrintCmd(c1,c2) }
   | PRINT APAR c1=command MAIS c2=command FPAR NOVALINHA 		{ ExprPrintCmd(c1,c2) }
   | PRINT APAR c1=command MAIS STR APAR c2=command FPAR FPAR NOVALINHA 		{ ExprPrintCmd(c1,c2) }
   | WHILE v=verificacao DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprWhileVerificacao(v,s) }
   | WHILE b=booleano DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprWhileBooleano(b,s) }
   | WHILE c=command DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprWhileCmd(c,s) }
   | FOR c1=command IN RANGE APAR c2=command FPAR DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprForRange(c1,c2,s) }
   | FOR c1=command IN c2=command DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprForId(c1,c2,s) }
   | IF v=verificacao DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprIf(v,s) }
   | ELIF v=verificacao DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprElif(v,s) }
   | ELSE DPONTOS NOVALINHA INDENTA s=seq+ DEDENTA 		{ ExprElse(s) }
   | RETURN c=command NOVALINHA 		{ ExprReturn(c) } 
   | STR APAR c=command FPAR NOVALINHA 		{ ExprStrCast(c) }
   | INT APAR c=command FPAR NOVALINHA		{ ExprIntCast(c) }
   ;

tiposPrimitivos:
	| DPONTOS BOOL 			{ BOOL }
	| DPONTOS INT 			{ INT }
	| DPONTOS FLOAT 		{ FLOAT }
	| DPONTOS STR 		{ STR }
	;

booleano:
	| TRUE 		{ Verdadeiro }
	| FALSE 		{ Falso }
	;

