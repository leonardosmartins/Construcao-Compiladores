
exception Error

let _eRR =
  Error

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

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState192
  | MenhirState189
  | MenhirState183
  | MenhirState173
  | MenhirState164
  | MenhirState163
  | MenhirState153
  | MenhirState151
  | MenhirState137
  | MenhirState136
  | MenhirState129
  | MenhirState128
  | MenhirState126
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState105
  | MenhirState104
  | MenhirState102
  | MenhirState101
  | MenhirState97
  | MenhirState96
  | MenhirState92
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState80
  | MenhirState78
  | MenhirState77
  | MenhirState73
  | MenhirState70
  | MenhirState62
  | MenhirState51
  | MenhirState49
  | MenhirState45
  | MenhirState43
  | MenhirState39
  | MenhirState35
  | MenhirState33
  | MenhirState27
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState9
  | MenhirState8
  | MenhirState1
  | MenhirState0
  
	open Ast


let rec _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.prog) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_nonempty_list_seq_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.seq list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.seq list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_seq_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, c), _, p), _, s) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.func) =                                                                                    ( DefFuncao(c,p,s) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let f = _v in
            let _v : (Ast.seq) =             ( Funcao(f) ) in
            _menhir_goto_seq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, v), _, s) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                    ( ExprElif(v,s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, s) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                      ( ExprElse(s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, c1), _), _, c2), _, s) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                                              ( ExprForRange(c1,c2,s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, c1), _, c2), _, s) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                              ( ExprForId(c1,c2,s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, v), _, s) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                  ( ExprIf(v,s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, v), _, s) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                     ( ExprWhileVerificacao(v,s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, c), _, s) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                 ( ExprWhileCmd(c,s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEDENTA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, b), _, s) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                  ( ExprWhileBooleano(b,s) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, s) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.prog) =                                ( Prog(s) ) in
            _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.prog) =                                          ( Prog(s) ) in
                _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, s) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.prog) =                      ( Prog(s) ) in
            _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, s) = _menhir_stack in
                let _3 = () in
                let _2 = () in
                let _v : (Ast.prog) =                                ( Prog(s) ) in
                _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_op : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.op) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState136 | MenhirState151 | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_parametro_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.parametro list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.parametro list) =     ( x :: xs ) in
        _menhir_goto_list_parametro_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DPONTOS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NOVALINHA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | INDENTA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | DEF ->
                            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | ELIF ->
                            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | ELSE ->
                            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | FALSE ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | FOR ->
                            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | FROM ->
                            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | ID _v ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                        | IF ->
                            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | IMPORT ->
                            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | INPUT ->
                            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | INT ->
                            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | LITFLOAT _v ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                        | LITINT _v ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                        | LITSTRING _v ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                        | PRINT ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | RETURN ->
                            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | STR ->
                            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | TRUE ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | WHILE ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_logica : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.logica) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | APAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
    | _ ->
        _menhir_fail ()

and _menhir_goto_comparador : _menhir_env -> 'ttv_tail -> (Ast.cmp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_goto_seq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ELIF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ELSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | FOR ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | FROM ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | IMPORT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | INPUT ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | INT ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | PRINT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | RETURN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | STR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | DEDENTA | EOF | NOVALINHA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.seq list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_seq_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119

and _menhir_goto_operacao : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.operacao) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState192 | MenhirState189 | MenhirState183 | MenhirState33 | MenhirState77 | MenhirState173 | MenhirState92 | MenhirState96 | MenhirState101 | MenhirState118 | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, o) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                            ( ExprOperacao(o) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, c), _), _, o) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) =                                            ( ExprAtribCmdOp(c,o) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.op) =            ( Multiplicacao ) in
    _menhir_goto_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_run122 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.op) =             ( Modulo ) in
    _menhir_goto_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.op) =            ( Menos ) in
    _menhir_goto_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.op) =           ( Mais ) in
    _menhir_goto_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.op) =               ( Dividido ) in
    _menhir_goto_op _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_tiposPrimitivos : _menhir_env -> 'ttv_tail -> (Ast.tiposPrimitivos) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let tp = _v in
    let (_menhir_stack, _menhir_s, c) = _menhir_stack in
    let _v : (Ast.parametro) =                                   ( Param(c,tp) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FPAR ->
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.parametro list) =     ( [] ) in
    _menhir_goto_list_parametro_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_verificacao : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.verificacao) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DPONTOS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INDENTA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEF ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | ELIF ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | ELSE ->
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | FOR ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | FROM ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                    | IF ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | IMPORT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | INPUT ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | INT ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                    | PRINT ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | RETURN ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | STR ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DPONTOS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INDENTA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEF ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | ELIF ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | ELSE ->
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | FOR ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | FROM ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                    | IF ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | IMPORT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | INPUT ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | INT ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                    | PRINT ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | RETURN ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | STR ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DPONTOS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INDENTA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEF ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | ELIF ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | ELSE ->
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | FOR ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | FROM ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | IF ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | IMPORT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | INPUT ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | INT ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | PRINT ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | RETURN ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | STR ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.logica) =         ( OULogico ) in
    _menhir_goto_logica _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.logica) =        ( ELogico ) in
    _menhir_goto_logica _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.cmp) =                 ( MenorIgual ) in
    _menhir_goto_comparador _menhir_env _menhir_stack _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.cmp) =            ( Menor ) in
    _menhir_goto_comparador _menhir_env _menhir_stack _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.cmp) =                 ( MaiorIgual ) in
    _menhir_goto_comparador _menhir_env _menhir_stack _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.cmp) =            ( Maior ) in
    _menhir_goto_comparador _menhir_env _menhir_stack _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.cmp) =                ( Igualdade ) in
    _menhir_goto_comparador _menhir_env _menhir_stack _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.logico_value) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, b) = _menhir_stack in
    let _v : (Ast.cmd) =                 ( BOOLEANO(b) ) in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let e = _v in
    let _v : (Ast.seq) =             ( Expressao(e) ) in
    _menhir_goto_seq _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_command : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.cmd) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState73 | MenhirState97 | MenhirState164 | MenhirState8 | MenhirState12 | MenhirState16 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IGUALDADE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MAIOR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MAIORIGUAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MENOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MENORIGUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, c1), o), _, c2) = _menhir_stack in
        let _v : (Ast.comparacao) =                                           ( Comparacao(c1,o,c2) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState8 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | E ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | OU ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
        | MenhirState12 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | E ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | OU ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState16 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | E ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | OU ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
        | MenhirState18 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((((_menhir_stack, _menhir_s), _, c1), _, l1), _, c2), _, l2), _, c3), _, l3), _, c4) = _menhir_stack in
                let _11 = () in
                let _7 = () in
                let _5 = () in
                let _1 = () in
                let _v : (Ast.verificacao) =                                                                                                                ( VerificacaoMultipla(c1,l1,c2,l2,c3,l3,c4) ) in
                _menhir_goto_verificacao _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState1 | MenhirState73 | MenhirState97 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | E ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | OU ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | DPONTOS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, c) = _menhir_stack in
                let _v : (Ast.verificacao) =                   ( Verificacao(c) ) in
                _menhir_goto_verificacao _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
        | MenhirState164 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, c1), _, l), _, c2) = _menhir_stack in
            let _v : (Ast.verificacao) =                                           ( VerificacaoDupla(c1,l,c2) ) in
            _menhir_goto_verificacao _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expr) =                                          ( ExprStrCast(c) ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                   ( ExprReturn(c) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expr) =                                            ( ExprPrint(c) ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MAIS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | STR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState49 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | APAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
        | VIRG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, c1), _, c2) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expr) =                                                             ( ExprPrintCmd(c1,c2) ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NOVALINHA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s), _, c1), _), _, c2) = _menhir_stack in
                    let _10 = () in
                    let _9 = () in
                    let _8 = () in
                    let _6 = () in
                    let _5 = () in
                    let _4 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ast.expr) =                                                                           ( ExprPrintCmd(c1,c2) ) in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, c1), _, c2) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expr) =                                                             ( ExprPrintCmd(c1,c2) ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expr) =                                         ( ExprIntCast(c) ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                  ( ExprImport(c) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IMPORT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, c1), _, c2) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                   ( ExprFromImport(c1,c2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | RANGE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState85 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | APAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DPONTOS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NOVALINHA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | INDENTA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | DEF ->
                            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | ELIF ->
                            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | ELSE ->
                            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | FALSE ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | FOR ->
                            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | FROM ->
                            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | ID _v ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                        | IF ->
                            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | IMPORT ->
                            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | INPUT ->
                            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | INT ->
                            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | LITFLOAT _v ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                        | LITINT _v ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                        | LITSTRING _v ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                        | PRINT ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | RETURN ->
                            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | STR ->
                            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | TRUE ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | WHILE ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | APAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | FPAR ->
                _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DPONTOS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.tiposPrimitivos) =                    ( BOOL ) in
                _menhir_goto_tiposPrimitivos _menhir_env _menhir_stack _v
            | FLOAT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.tiposPrimitivos) =                    ( FLOAT ) in
                _menhir_goto_tiposPrimitivos _menhir_env _menhir_stack _v
            | INT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.tiposPrimitivos) =                   ( INT ) in
                _menhir_goto_tiposPrimitivos _menhir_env _menhir_stack _v
            | STR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.tiposPrimitivos) =                  ( STR ) in
                _menhir_goto_tiposPrimitivos _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDIDO ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MAIS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MENOS ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MODULO ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | VEZES ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, op), _, o), _, c) = _menhir_stack in
            let _v : (Ast.operacao) =                                    ( OperacaoComando(op,o,c) ) in
            _menhir_goto_operacao _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, c1), _, o), _, c2) = _menhir_stack in
        let _v : (Ast.operacao_op) =                                   ( Operacao_op(c1,o,c2) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState0 | MenhirState192 | MenhirState189 | MenhirState183 | MenhirState33 | MenhirState77 | MenhirState173 | MenhirState92 | MenhirState96 | MenhirState101 | MenhirState118 | MenhirState137 | MenhirState119 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DIVIDIDO ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | MAIS ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | MENOS ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | MODULO ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | VEZES ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, op) = _menhir_stack in
                let _v : (Ast.operacao) =                       ( Operacao(op) ) in
                _menhir_goto_operacao _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
        | MenhirState126 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, op1), _, o), _, op2) = _menhir_stack in
            let _v : (Ast.operacao) =                                           ( OperacaoOperacao(op1,o,op2) ) in
            _menhir_goto_operacao _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState0 | MenhirState192 | MenhirState189 | MenhirState183 | MenhirState33 | MenhirState77 | MenhirState173 | MenhirState92 | MenhirState96 | MenhirState101 | MenhirState118 | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ATRIB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState136 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | INPUT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState137 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | APAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FPAR ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | NOVALINHA ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, c), _), _) = _menhir_stack in
                            let _6 = () in
                            let _5 = () in
                            let _4 = () in
                            let _3 = () in
                            let _2 = () in
                            let _v : (Ast.expr) =                                                  ( ExprAtribCmdInput(c) ) in
                            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (_menhir_stack, _menhir_s) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | INT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState137 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | APAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | INPUT ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | APAR ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | FPAR ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | FPAR ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | NOVALINHA ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let (((_menhir_stack, _menhir_s, c), _), _) = _menhir_stack in
                                        let _9 = () in
                                        let _8 = () in
                                        let _7 = () in
                                        let _6 = () in
                                        let _5 = () in
                                        let _4 = () in
                                        let _3 = () in
                                        let _2 = () in
                                        let _v : (Ast.expr) =                                                                ( ExprAtribCmdInput(c) ) in
                                        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (_menhir_stack, _menhir_s) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (_menhir_stack, _menhir_s) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
        | DIVIDIDO ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MAIS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MENOS ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MODULO ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | VEZES ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | DEDENTA | DEF | ELIF | ELSE | EOF | FALSE | FOR | FROM | ID _ | IF | IMPORT | INPUT | INT | LITFLOAT _ | LITINT _ | LITSTRING _ | NOVALINHA | PRINT | RETURN | STR | TRUE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, c) = _menhir_stack in
            let _v : (Ast.seq) =                ( Comando(c) ) in
            _menhir_goto_seq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | APAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState151 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | FPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState153 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NOVALINHA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _menhir_s, c1), _), _, c2), _), _) = _menhir_stack in
                    let _6 = () in
                    let _5 = () in
                    let _4 = () in
                    let _2 = () in
                    let _v : (Ast.expr) =                                                        ( ExprAtribCmdCmd(c1,c2) ) in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | LITFLOAT _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | LITINT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | LITSTRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
        | DIVIDIDO ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | MAIS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | MENOS ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | MODULO ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState151 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, c1), _), _, c2) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) =                                              ( ExprAtribCmdCmd(c1,c2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | VEZES ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s, c1), _), _, c2), _), _, c3) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _v : (Ast.expr) =                                                                  ( ExprAtribCmdCmdCmd(c1,c2,c3) ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DPONTOS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INDENTA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEF ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | ELIF ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | ELSE ->
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | FOR ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | FROM ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
                    | IF ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | IMPORT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | INPUT ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | INT ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
                    | PRINT ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | RETURN ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | STR ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState173
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DPONTOS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INDENTA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEF ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | ELIF ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | ELSE ->
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | FOR ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | FROM ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                    | IF ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | IMPORT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | INPUT ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | INT ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                    | PRINT ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | RETURN ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | STR ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | IGUALDADE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | MAIOR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MAIORIGUAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MENOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MENORIGUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_booleano : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.logico_value) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState192 | MenhirState189 | MenhirState183 | MenhirState33 | MenhirState73 | MenhirState77 | MenhirState83 | MenhirState85 | MenhirState173 | MenhirState87 | MenhirState92 | MenhirState96 | MenhirState97 | MenhirState164 | MenhirState101 | MenhirState102 | MenhirState104 | MenhirState118 | MenhirState119 | MenhirState137 | MenhirState153 | MenhirState126 | MenhirState129 | MenhirState105 | MenhirState78 | MenhirState80 | MenhirState70 | MenhirState62 | MenhirState43 | MenhirState49 | MenhirState51 | MenhirState45 | MenhirState39 | MenhirState35 | MenhirState8 | MenhirState12 | MenhirState16 | MenhirState18 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DPONTOS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INDENTA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEF ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | ELIF ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | ELSE ->
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | FALSE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | FOR ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | FROM ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                    | IF ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | IMPORT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | INPUT ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | INT ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | LITFLOAT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                    | LITINT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                    | LITSTRING _v ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                    | PRINT ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | RETURN ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | STR ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | TRUE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | IGUALDADE | MAIOR | MAIORIGUAL | MENOR | MENORIGUAL ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APAR ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.logico_value) =           ( Verdadeiro ) in
    _menhir_goto_booleano _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let l = _v in
    let _v : (Ast.cmd) =                  ( LITSTRING(l) ) in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let l = _v in
    let _v : (Ast.cmd) =               ( LITINT(l) ) in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let l = _v in
    let _v : (Ast.cmd) =                 ( LITFLOAT(l) ) in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NOVALINHA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expr) =                                 ( ExprInput ) in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APAR ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (Ast.cmd) =         ( ID(i) ) in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.logico_value) =            ( Falso ) in
    _menhir_goto_booleano _menhir_env _menhir_stack _menhir_s _v

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DPONTOS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NOVALINHA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INDENTA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEF ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | ELIF ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | ELSE ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | FALSE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | FOR ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | FROM ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | ID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | IF ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | IMPORT ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | INPUT ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | INT ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | LITFLOAT _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | LITINT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | LITSTRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | PRINT ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | RETURN ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | STR ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | TRUE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APAR ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ELIF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ELSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FOR ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FROM ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IMPORT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INPUT ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LITFLOAT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LITINT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LITSTRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | NOVALINHA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | ELIF ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | ELSE ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | FALSE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | FOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | FROM ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | IF ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | IMPORT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | INPUT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | INT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | LITFLOAT _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | LITINT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | LITSTRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | PRINT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | RETURN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | STR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
    | PRINT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RETURN ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

