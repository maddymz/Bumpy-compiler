compileBumpy(FileName) :- open(FileName, read, InStream),
		   tokenCodes(InStream, TokenCodes),
		   phrase(lexer(Tokens),TokenCodes),
		   parser(ParseTree, Tokens, []),
		   close(InStream),
		   split_string(FileName, ".", "", L),
		   L = [H|_T],
		   atom_concat(H, ".bmic", X),
		   open(X, write, OutStream),
		   write(OutStream, ParseTree),
		   write(OutStream, '.'),
close(OutStream).


tokenCodes(InStream,[]) :- at_end_of_stream(InStream), !.
tokenCodes(InStream, [TokenCode | RemTokens]) :- get_code(InStream, TokenCode),
                                                 tokenCodes(InStream, RemTokens).


%===========================================================================================

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
        "show", !, {Token = show};
        "input", !, {Token = input};
        "~=", !, { Token = ~= };
        "+",  !, { Token = +  };
        "-",  !, { Token = -  };
        "*",  !, { Token = *  };
        "/", !, { Token = /  };
        "$", !, {Token = $ };
        "mod", !, { Token = mod  };  
        digit(D),  !, number(D, N), { Token = N };
        lowletter(L), !, identifier(L, Id),{  Token = Id};
        upletter(L), !, identifier(L, Id), { Token = Id };
        [Un], { Token = tkUnknown, throw((unrecognized_token, Un)) }),!,
        { Tokens = [Token | TokList] },lexer(TokList);  
    	  [],{ Tokens = [] }).

white_space --> [Char], { code_type(Char,space) }, !, white_space.
white_space --> [].

digit(Digit) --> [Digit],{ code_type(Digit, digit) }.
digits([Digit|DigitTail]) --> digit(Digit),!,digits(DigitTail).
digits([]) -->[].

number(Digit, Number) --> digits(DigitRem),{ number_chars(Number, [Digit|DigitRem]) }.

upletter(Letter) -->[Letter], { code_type(Letter, upper) }.

lowletter(Letter) -->[Letter], { code_type(Letter, lower) }.

alphanum([Alphanum|AlphaTail]) -->[Alphanum], { code_type(Alphanum, csym) }, !, alphanum(AlphaTail).
alphanum([]) -->[].

identifier(IdentList, Ident) -->alphanum(IdentTail),{ atom_codes(Ident, [IdentList|IdentTail]) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parser(ProgramNode) --> program(ProgramNode).

program(t_program(CommentNode,BlockNode)) -->comment(CommentNode),block(BlockNode).
program(t_program(BlockNode)) -->block(BlockNode).

comment(t_comment(WordNode)) --> [@],words(WordNode),[@].

words(t_words(IdentNumNode,WordNode)) --> identifier(IdentNumNode), words(WordNode) ; numb(IdentNumNode), words(WordNode).
words(t_words(IdentNumNode)) -->identifier(IdentNumNode); numb(IdentNumNode).

block(t_block(DeclareNode,ProcessNode)) --> [start], declaration(DeclareNode), process(ProcessNode), [stop].

datatype(t_datatype(var)) --> [var].
datatype(t_datatype(bool)) --> [bool].

declaration(t_declare(DataTypeNode,IdentifierNode,DeclareNode)) --> datatype(DataTypeNode), identifier(IdentifierNode), [;], declaration(DeclareNode).    
declaration(t_declare(DataTypeNode,IdentifierNode)) -->datatype(DataTypeNode),identifier(IdentifierNode),[;].

process(t_process(ConstructNode,ProcessNode)) --> assignvalue(ConstructNode), [;], process(ProcessNode); control(ConstructNode), process(ProcessNode); iterate(ConstructNode), process(ProcessNode); print(ConstructNode), process(ProcessNode);
                            readValue(ConstructNode), process(ProcessNode).
process(t_process(ConstructNode)) -->assignvalue(ConstructNode),[;] ;control(ConstructNode) ;iterate(ConstructNode);print(ConstructNode);readValue(ConstructNode).

readValue(t_read(IdentifierNode)) --> [input], identifier(IdentifierNode), [;].

print(t_print(ExpressionNode)) --> [show],expression(ExpressionNode),[;].
print(t_printString(ValueNode)) --> [show], [*], value(ValueNode), [*], [;], !.

value(ValueNode,ListNode,RemListNode):-stringCreate(PrintListNode,ListNode,RemListNode),atomic_list_concat(PrintListNode," ",ValueNode).

stringCreate(PrintListNode, ListNode, RemListNode) :- 
    ListNode = [Head|Tail],
    Head \= '*',
    stringCreate(Final,Tail,RemListNode),
    PrintListNode = [Head|Final].
stringCreate([],ListNode,ListNode) :- ListNode = [Head|_], Head = '*'.

assignvalue(t_assign(IdentifierNode,ExpressionNode)) --> identifier(IdentifierNode), [=] ,expression(ExpressionNode); identifier(IdentifierNode), [is], boolexp(ExpressionNode).

control(t_control(ConditionNode,ProcessNode,ProcessNode1)) --> [incase], condition(ConditionNode), [do], process(ProcessNode), [otherwise], process(ProcessNode1), [endcase].

iterate(t_iterate(ConditionNode,ProcessNode)) --> [when], condition(ConditionNode), [repeat], process(ProcessNode), [endrepeat].

condition(t_cond_and(ExpressionNode,ExpressionNode1)) --> boolexp(ExpressionNode), [and], boolexp(ExpressionNode1).
condition(t_cond_or(ExpressionNode,ExpressionNode1)) --> boolexp(ExpressionNode), [or], boolexp(ExpressionNode1).
condition(t_cond_not(ExpressionNode)) --> [~], boolexp(ExpressionNode).
condition(t_cond(ExpressionNode)) --> boolexp(ExpressionNode).

boolexp(t_boolexp_eq(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [:=:], expression(ExpressionNode1).
boolexp(t_boolexp_neq(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [~=], expression(ExpressionNode1). 
boolexp(t_boolexp_leq(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [<],[=], expression(ExpressionNode1).
boolexp(t_boolexp_geq(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [>],[=], expression(ExpressionNode1).
boolexp(t_boolexp_less(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [<], expression(ExpressionNode1).
boolexp(t_boolexp_great(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [>], expression(ExpressionNode1).
boolexp(t_boolexp_beq(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [:=:], boolexp(ExpressionNode1).
boolexp(t_boolexp_bneq(ExpressionNode,ExpressionNode1)) --> expression(ExpressionNode), [~=], boolexp(ExpressionNode1). 
boolexp(t_boolexp(yes)) --> [yes].
boolexp(t_boolexp(no)) --> [no].

expression(t_add(TermNode,ExpressionNode)) --> term(TermNode),[+],expression(ExpressionNode).
expression(t_sub(TermNode,ExpressionNode)) --> term(TermNode),[-],expression(ExpressionNode).
expression(t_expr(TermNode)) --> term(TermNode).

term(t_mul(IdentNumNode,TermNode)) --> identifier(IdentNumNode),[*],term(TermNode);numb(IdentNumNode),[*],term(TermNode);numbneg(IdentNumNode),[*],term(TermNode).
term(t_div(IdentNumNode,TermNode)) -->identifier(IdentNumNode),[/],term(TermNode);numb(IdentNumNode),[/],term(TermNode);numbneg(IdentNumNode),[/],term(TermNode).
term(t_mod(IdentNumNode,TermNode)) -->identifier(IdentNumNode),[mod],term(TermNode);numb(IdentNumNode),[mod],term(TermNode);numbneg(IdentNumNode),[mod],term(TermNode).
term(t_term(IdentNumNode)) -->identifier(IdentNumNode);numb(IdentNumNode);numbneg(IdentNumNode).

identifier(t_identifier(IdentifierNode)) -->[IdentifierNode], 
    {string_chars(IdentifierNode,[IdentHead|IdentTail])}, 
    {(is_alpha(IdentHead); IdentifierNode = '_')},
    {forall(member(Char,IdentTail),
    (is_alnum(Char)); Char = '_')}.

numbneg(t_numbneg(NumNode)) --> [-],numb(NumNode).
numb(t_numb(NumNode)) --> [NumNode],{number(NumNode)}.
