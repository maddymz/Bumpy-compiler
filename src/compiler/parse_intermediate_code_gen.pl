tokenCodes(InStream, []) :- at_end_of_stream(InStream), !.
tokenCodes(InStream, [TokenCode|RemTokens]) :- get_code(InStream, TokenCode), tokenCodes(InStream, RemTokens).


%===========================================================================================
%Tokenizer implemented with the list of keywords and other values

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

white_space --> [Char], {code_type(Char,space) }, !, white_space.
white_space --> [].

digit(Digit) --> [Digit], {code_type(Digit, digit) }.
digits([Digit|DigitTail]) --> digit(Digit), !, digits(DigitTail).
digits([]) --> [].

number(Digit, Number) --> digits(DigitRem), {number_chars(Number, [Digit|DigitRem])}.

upletter(Letter) --> [Letter], { code_type(Letter, upper)}.

lowletter(Letter) --> [Letter], { code_type(Letter, lower)}.

alphanum([Alphanum|AlphaTail]) --> [Alphanum], {code_type(Alphanum, csym)}, !, alphanum(AlphaTail).
alphanum([]) --> [].

identifier(IdentList, Ident) --> alphanum(IdentTail), {atom_codes(Ident, [IdentList|IdentTail])}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Initializing Parser, Takes tokens from lexer and generates parse tree 

parser(ProgramNode) --> program(ProgramNode).
program(t_program(CommentNode,BlockNode)) --> comment(CommentNode), block(BlockNode).
program(t_program(BlockNode)) --> block(BlockNode).

%Rule for comment 
comment(t_comment(WordNode)) --> [@], words(WordNode), [@].

%Rules for comment content
words(t_words(IdentifierNode, WordNode)) --> identifier(IdentifierNode), words(WordNode).
words(t_words(NumberNode, WordNode)) --> numb(NumberNode), words(WordNode).
words(t_words(IdentifierNode)) --> identifier(IdentifierNode).
words(t_words(NumberNode)) --> numb(NumberNode).

%Rule for Block
block(t_block(DeclareNode, ProcessNode)) --> [start], declaration(DeclareNode), process(ProcessNode), [stop].

%Rules for datatypes
datatype(t_datatype(var)) --> [var].
datatype(t_datatype(bool)) --> [bool].

%Rules for declaration
declaration(t_declare(DataTypeNode, IdentifierNode, DeclareNode)) --> datatype(DataTypeNode), identifier(IdentifierNode), [;], declaration(DeclareNode).    
declaration(t_declare(DataTypeNode, IdentifierNode)) --> datatype(DataTypeNode), identifier(IdentifierNode), [;].

%Rules for process
process(t_process(AssignNode, ProcessNode)) --> assignvalue(AssignNode), process(ProcessNode).
process(t_process(ControlNode, ProcessNode)) --> control(ControlNode), process(ProcessNode).
process(t_process(LoopNode, ProcessNode)) --> iterate(LoopNode), process(ProcessNode).
process(t_process(PrintNode, ProcessNode)) --> print(PrintNode), process(ProcessNode).
process(t_process(ReadNode, ProcessNode)) --> readValue(ReadNode), process(ProcessNode).
process(t_process(AssignNode)) --> assignvalue(AssignNode).
process(t_process(ControlNode)) --> control(ControlNode).
process(t_process(LoopNode)) --> iterate(LoopNode).
process(t_process(PrintNode)) --> print(PrintNode).
process(t_process(ReadNode)) --> readValue(ReadNode).

%Rule for reading values
readValue(t_read(IdentifierNode)) --> [input], identifier(IdentifierNode), [;].

%Rules for printing values
print(t_print(ExpressionNode)) --> [show], expression(ExpressionNode),[;].
print(t_printString(X)) --> [show], [*], value(X), [*], [;], !.

value(X,Y,Z):-stringCreate(L,Y,Z),atomic_list_concat(L," ",X).

stringCreate(PrintList, List, Rem) :- 
    List = [H|T],
    H \= '*',
    stringCreate(X,T,Rem),
    PrintList = [H|X].
stringCreate([],List,List) :- List = [H|_], H = '*'.

%Rules for assigning values
assignvalue(t_assign(IdentifierNode, ExpressionNode)) --> identifier(IdentifierNode), [=] ,expression(ExpressionNode), [;].
assignvalue(t_assign(IdentifierNode, BooleanExpressionNode)) -->  identifier(IdentifierNode), [is], boolexp(BooleanExpressionNode), [;].

%Rules for control structure
control(t_control(ConditionNode, TrueProcessNode, FalseProcessNode)) --> [incase], condition(ConditionNode), 
                                                                        [do], process(TrueProcessNode), [otherwise],
                                                                        process(FalseProcessNode), [endcase].

%Rules for loop structure
iterate(t_iterate(ConditionNode, ProcessNode)) --> [when], condition(ConditionNode), [repeat], process(ProcessNode), [endrepeat].

%Rules for conditions
condition(t_cond_and(BooleanExpressionNode1, BooleanExpressionNode2)) --> boolexp(BooleanExpressionNode1), [and], boolexp(BooleanExpressionNode2).
condition(t_cond_or(BooleanExpressionNode1, BooleanExpressionNode2)) --> boolexp(BooleanExpressionNode1), [or], boolexp(BooleanExpressionNode2).
condition(t_cond_not(BooleanExpressionNode)) --> [~], boolexp(BooleanExpressionNode).
condition(t_cond(BooleanExpressionNode)) --> boolexp(BooleanExpressionNode).

%Rules for Boolean expressions 
boolexp(t_boolexp_eq(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [:=:], expression(ExpressionNode2).
boolexp(t_boolexp_neq(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [~=], expression(ExpressionNode2). 
boolexp(t_boolexp_leq(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [<],[=], expression(ExpressionNode2).
boolexp(t_boolexp_geq(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [>],[=], expression(ExpressionNode2).
boolexp(t_boolexp_less(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [<], expression(ExpressionNode2).
boolexp(t_boolexp_great(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [>], expression(ExpressionNode2).
boolexp(t_boolexp_beq(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [:=:], boolexp(ExpressionNode2).
boolexp(t_boolexp_bneq(ExpressionNode1, ExpressionNode2)) --> expression(ExpressionNode1), [~=], boolexp(ExpressionNode2). 
boolexp(t_boolexp(yes)) --> [yes].
boolexp(t_boolexp(no)) --> [no].

%Rules for expressions
expression(t_add(TermNode, ExpressionNode)) --> term(TermNode), [+], expression(ExpressionNode).
expression(t_sub(TermNode, ExpressionNode)) --> term(TermNode), [-], expression(ExpressionNode).
expression(t_expr(TermNode)) --> term(TermNode).

term(t_mul(IdentifierNode, TermNode)) --> identifier(IdentifierNode), [*], term(TermNode).
term(t_mul(NumberNode, TermNode)) --> numb(NumberNode),[*], term(TermNode).
term(t_mul(NegNumberNode, TermNode)) --> numbneg(NegNumberNode), [*], term(TermNode).
term(t_div(IdentifierNode, TermNode)) --> identifier(IdentifierNode), [/], term(TermNode).
term(t_mul(NumberNode, TermNode)) --> numb(NumberNode), [/], term(TermNode).
term(t_mul(NegNumberNode, TermNode)) --> numbneg(NegNumberNode), [/], term(TermNode).
term(t_mod(IdentifierNode, TermNode)) -->identifier(IdentifierNode), [mod], term(TermNode).
term(t_mul(NumberNode, TermNode)) --> numb(NumberNode), [mod], term(TermNode).
term(t_mul(NegNumberNode, TermNode)) --> numbneg(NegNumberNode), [mod], term(TermNode).
term(t_term(IdentifierNode)) --> identifier(IdentifierNode).
term(t_term(NumberNode)) --> numb(NumberNode).
term(t_term(NegNumberNode)) --> numbneg(NegNumberNode).

%Rule for Identifier
identifier(t_identifier(IdentifierNode)) --> [IdentifierNode], 
    {string_chars(IdentifierNode, [IdentHead|IdentTail])}, 
    {(is_alpha(IdentHead); IdentifierNode = '_')},
    {forall(member(Char, IdentTail),
    (is_alnum(Char)); Char = '_')}.

%Rules for numbers
numbneg(t_numbneg(NumberNode)) --> [-],numb(NumberNode).
numb(t_numb(NumberNode)) --> [NumberNode],{number(NumberNode)}.