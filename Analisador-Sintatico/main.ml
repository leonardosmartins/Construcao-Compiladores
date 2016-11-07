
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast  = Parser.prog Pre_processador.lexico lexbuf in
  ast	  
(*let _ = Parser.prog Pre_processador.lexico lexbuf in
  ast*)
;;

let parse_arq arq =
  let ic = open_in arq in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parser.prog Pre_processador.lexico lexbuf in
  ast	
  (*let _ = Parser.prog Pre_processador.lexico lexbuf in
  print_endline "FUNCAO PASSOU NO TESTE"*)
;;


(*fffffff*)

type nome_arq = string
type tokens = Parser.token list

let rec tokens lexbuf =
  let tok = Pre_processador.lexico lexbuf in
  match tok with
  | Parser.EOF -> ([Parser.EOF]:tokens)
  | _ -> tok :: tokens lexbuf
;;

let lex str =
  let lexbuf = Lexing.from_string str in
  tokens lexbuf
;;

let lex_arq (arq:nome_arq) =
  let ic = open_in arq in
  let lexbuf = Lexing.from_channel ic in
  let toks = tokens lexbuf in
  let _ = close_in ic in
  toks

(*ffffffff*)
(* let testes () =
  assert (() = parse "	def func():
  							x = 9;
  						if x ==9: 
  							return 3;
					 	else:
					  		x = x +1;");
  
  assert(() = parse "def micro():
						numero =0;
						print(\"expr\");
						numero = input();
						if numero>= 100:
							if numero<= 200:
								print(\"expr\");
							else:
								print(\"expr\");
						else:
							print(\"expr\");");

  assert (() = parse "def nano():
						pass;");


  assert (() = parse "def nano():
						n = 2 ;
						print(\"n\",n);");

  assert (() = parse "def micro():
						numero = 0;
						x =0;
						print(\"numero\");
						numero = input();
						if x ==1:
							print(\"Positivo\");
						if x ==0:
							print(\"zero\");
						else:
							print(\"Negativo\");

					def verifica(n):
						res = 0;
						if n>0:
							res = 1;
						if n<0:
							res = -1;
						else:
							res = 0;

						return res;");

  assert (() = parse "while x < y:
  						print(\"diferenca\", x < y);
  						");

  assert (() = parse "for x in range(len(y)):
  						if x != 2:
  							x =1;");

  assert (() = parse "def nano():
						n=1;
						m=2;
						x=5;
						while x > n:
							n = n + m;
							print(\"n\",n);
");

 assert (() = parse "(10+1)+(5+6);"); (*essa ainda nao funciona*)
  
 *)
