type identificador = string

type prog = Prog of seq list

and seq = Expressao of expr
		| Comando of cmd
		| Funcao of func

and cmd = LITINT of int
	    | LITFLOAT of float
	    | LITSTRING of string
	    | BOOLEANO of logico_value
	    | ID of string

and logico_value = Verdadeiro
				 | Falso

and parametro = Param of cmd * tiposPrimitivos

and tiposPrimitivos = BOOL 
			        | INT
			        | FLOAT
			        | STR

and func = DefFuncao of cmd * parametro list * seq list  

and op = Mais
	   | Menos
	   | Dividido
	   | Multiplicacao
	   | Modulo

and operacao_op = Operacao_op of cmd * op * cmd

and operacao = Operacao of operacao_op
			 | OperacaoOperacao of operacao_op * op * operacao_op
			 | OperacaoComando of operacao_op * op * cmd

and cmp = MenorIgual
		| Igualdade
		| MaiorIgual
		| Maior
		| Menor

and comparacao = Comparacao of cmd * cmp * cmd

and logica = ELogico
		   | OULogico

and verificacao = Verificacao of comparacao
		        | VerificacaoDupla of comparacao * logica * comparacao
		        | VerificacaoMultipla of comparacao * logica * comparacao * logica  * comparacao * logica * comparacao

and expr = ExprImport of cmd
		 | ExprFromImport of cmd * cmd
		 | ExprOperacao of operacao	
		 | ExprInput
		 | ExprAtribCmdOp of cmd * operacao    
		 | ExprAtribCmdCmd of cmd * cmd
		 | ExprAtribCmdCmdCmd of cmd * cmd * cmd
		 | ExprAtribCmdInput of cmd
		 | ExprPrint of cmd
		 | ExprPrintCmd of cmd * cmd
		 | ExprWhileVerificacao of verificacao * seq list
		 | ExprWhileBooleano of logico_value * seq list
		 | ExprWhileCmd of cmd * seq list
		 | ExprForRange of cmd *cmd * seq list
		 | ExprForId of cmd * cmd * seq list
		 | ExprIf of verificacao * seq list
		 | ExprElif of verificacao * seq list
		 | ExprElse of seq list
		 | ExprReturn of cmd
		 | ExprStrCast of cmd
		 | ExprIntCast of cmd

