lexer(Tokens) -->
    white_space,
    (   ( ";",  !, { Token = ; };  
        "@",  !, { Token = @ };  
        "start",  !, { Token = start };
        "stop",  !, { Token = stop };
        "when",  !, { Token = when };
        "repeat",  !, { Token = repeat };
        "endrepeat",  !, { Token = endrepeat };
        "incase",  !, { Token = incase };
        "do",  !, { Token = do };
        "otherwise",  !, { Token = otherwise };  
        "endcase",  !, { Token = endcase };
        "yes",  !, { Token = yes };
        "no",  !, { Token = no };
        "and",  !, { Token = and };
        "or",  !, { Token = or };
        "var",  !, { Token = var };
        "bool",  !, { Token = bool };
        ">",  !, { Token = > };
        "<",  !, { Token = < };
        "<=", !, { Token = <= };
        ">=", !, { Token = >= };
        "=",  !, { Token = =  };
        "is",  !, { Token = is  };
        ":=:",  !, { Token = :=:  };
        "~=", !, { Token = ~= };
        "+",  !, { Token = +  };
        "-",  !, { Token = -  };
        "*",  !, { Token = *  };
        "/", !, { Token = /  };
        "mod", !, { Token = mod  };  
        digit(D),  !, number(D, N), { Token = N };
        lowletter(L), !, identifier(L, Id),{  Token = Id};
        upletter(L), !, identifier(L, Id), { Token = Id };
        [Un], { Token = tkUnknown, throw((unrecognized_token, Un)) }),!,
        { Tokens = [Token | TokList] },lexer(TokList);  
    	  [],{ Tokens = [] }).

white_space --> [Char], { code_type(Char,space) }, !, white_space.
white_space --> [].

digit(D) --> [D],{ code_type(D, digit) }.
digits([D|T]) --> digit(D),!,digits(T).
digits([]) -->[].

number(D, N) --> digits(Ds),{ number_chars(N, [D|Ds]) }.

upletter(L) -->[L], { code_type(L, upper) }.

lowletter(L) -->[L], { code_type(L, lower) }.

alphanum([A|T]) -->[A], { code_type(A, csym) }, !, alphanum(T).
alphanum([]) -->[].

identifier(L, Id) -->alphanum(As),{ atom_codes(Id, [L|As]) }.



% ?- phrase(lexer(T),`@ test program 3 @
%start
%    var n;
%    var i;
%    when yes
%        repeat 
%            n = n + 1;
%        endrepeat
%stop`).

