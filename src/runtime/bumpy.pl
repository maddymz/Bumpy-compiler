:- style_check(-singleton).

bumpy(FileName) :- open(FileName, read, InStream),
              tokenCodes(InStream, TokenCodes),
               phrase(lexer(Tokens), TokenCodes),
              parser(ParseTree, Tokens, []),
              close(InStream),
              split_string(FileName, ".", "", L),
              L = [H|_T],
              atom_concat(H, ".bmic", X),
              open(X, write, OutStream),
              write(OutStream, ParseTree),
              write(OutStream, '.'),
              close(OutStream),
              evalParser(ParseTree, EnvOut).

%===========================================================================================

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
print(t_printString(ValueNode)) --> [show], [*], value(ValueNode), [*], [;], !.

value(ValueNode, ListNode, RemListNode) :- stringCreate(PrintListNode, ListNode, RemListNode), atomic_list_concat(PrintListNode," ", ValueNode).

stringCreate(PrintListNode, ListNode, RemListNode) :- 
    ListNode = [Head|Tail],
    Head \= '*',
    stringCreate(Final, Tail, RemListNode),
    PrintListNode = [Head|Final].
stringCreate([], ListNode, ListNode) :- ListNode = [Head|_], Head = '*'.

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

%================================================================================================
%Interpretter goes as follows

%Rules to evaluate the parser.
evalParser(ProgramNode, EnvOut) :- evalProgram(ProgramNode,[],EnvOut).

% Rules to read statements.
evalRead(t_read(IdentifierNode), EnvIn, EnvOut):- read(Term), evalIdentifier(IdentifierNode,_,EnvIn,EnvIn2,IdentName), 
                                        update(IdentName, Term, EnvIn2, EnvOut).

% Look up the environment to find the value of an identifier
lookup(_,[],0).
lookup(Key,[(Key, Value)|_],Value).
lookup(Key,[_|Tail],Value) :- lookup(Key,Tail,Value).

% Update the environment with new value of an identifier.
update(Key,Value,[],[(Key,Value)]).
update(Key,Value,[(Key,_)|Tail],[(Key,Value)|Tail]).
update(Key,Value,[Head|Tail],[Head|Tail1]) :- update(Key,Value,Tail,Tail1).

% Rules to evaluate print statements.
evalPrint(t_print(ExpressionNode),EnvIn,EnvOut) :-  evalExpression(ExpressionNode,Output,EnvIn,EnvOut), write(Output).
evalPrint(t_printString(ValueNode), EnvIn, EnvIn) :- write(ValueNode), write(" "),!.

%Rules to evaluate program.
evalProgram(t_program(_,BlockNode),EnvIn, EnvOut) :-evalBlock(BlockNode,EnvIn, EnvOut),!.
evalProgram(t_program(BlockNode),EnvIn, EnvOut) :- evalBlock(BlockNode,EnvIn, EnvOut),!.

%Rules to evaluate block statements.
evalBlock(t_block(DeclareNode,ProcessNode),EnvIn,EnvOut) :-
evalDeclaration(DeclareNode,EnvIn,EnvIn2),
evalProcess(ProcessNode,EnvIn2,EnvOut).

%Rules to evaluate data type statements.
evalDatatype(t_datatype(var),Output,EnvIn,EnvIn):-Output=var,!.
evalDatatype(t_datatype(bool),Output,EnvIn,EnvIn):-Output=bool,!.


%Rules to evaluate declaration statements.
evalDeclaration(t_declare(DataTypeNode,IdentifierNode),EnvIn,EnvOut) :-
evalDatatype(DataTypeNode,_,EnvIn,EnvIn),
evalIdentifier(IdentifierNode,_,EnvIn,EnvIn,IdentName),update(IdentName,0, EnvIn, EnvOut).

evalDeclaration(t_declare(DataTypeNode,IdentifierNode,DeclareNode),EnvIn,EnvOut) :-
evalDatatype(DataTypeNode,_,EnvIn,EnvIn),
evalIdentifier(IdentifierNode,_,EnvIn,EnvIn,IdentName),update(IdentName,0, EnvIn, EnvIn2),
evalDeclaration(DeclareNode,EnvIn2,EnvOut).

%Rules to evaluate process statements.
evalProcess(t_process(ConstructNode),EnvIn,EnvOut):-evalPrint(ConstructNode,EnvIn, EnvOut),!.
evalProcess(t_process(ConstructNode),EnvIn,EnvOut):-evalAssign(ConstructNode,EnvIn,EnvOut),!.
evalProcess(t_process(ConstructNode),EnvIn,EnvOut):-evalIterate(ConstructNode,EnvIn,EnvOut),!.
evalProcess(t_process(ConstructNode),EnvIn,EnvOut):-evalControl(ConstructNode,EnvIn,EnvOut),!.
evalProcess(t_process(ConstructNode),EnvIn,EnvOut):-evalRead(ConstructNode,EnvIn,EnvOut),!.
evalProcess(t_process(ConstructNode,ProcessNode),EnvIn,EnvOut):-evalAssign(ConstructNode,EnvIn,EnvIn2),
                                                                evalProcess(ProcessNode,EnvIn2,EnvOut),!.
evalProcess(t_process(ConstructNode,ProcessNode),EnvIn,EnvOut):-evalIterate(ConstructNode,EnvIn,EnvIn2),
                                                                evalProcess(ProcessNode,EnvIn2,EnvOut),!.
evalProcess(t_process(ConstructNode,ProcessNode),EnvIn,EnvOut):-evalControl(ConstructNode,EnvIn,EnvIn2),
                                                                evalProcess(ProcessNode,EnvIn2,EnvOut),!.
evalProcess(t_process(ConstructNode,ProcessNode), EnvIn, EnvOut) :- evalPrint(ConstructNode,EnvIn,EnvIn2),
                                                                    evalProcess(ProcessNode,EnvIn2, EnvOut),!.
evalProcess(t_process(ConstructNode,ProcessNode), EnvIn, EnvOut) :- evalRead(ConstructNode, EnvIn, EnvIn2),
                                                                    evalProcess(ProcessNode,EnvIn2,EnvOut).

% Rules to evaluate assignment statements.
evalAssign(t_assign(IdentifierNode,ExpressionNode),EnvIn,EnvOut):-evalIdentifier(IdentifierNode,_,EnvIn,EnvIn,IdentName),
                                                                  evalExpression(ExpressionNode,Output,EnvIn,EnvIn),
                                                                  update(IdentName,Output, EnvIn,EnvOut),!.

evalAssign(t_assign(IdentifierNode,ExpressionNode),EnvIn,EnvOut):-evalIdentifier(IdentifierNode,_,EnvIn,EnvIn,IdentName),
                                                                  evalBoolexp(ExpressionNode,Output,EnvIn,EnvIn),
                                                                  update(IdentName,Output, EnvIn,EnvOut),!.

%Rules to evaluate control statements.
evalControl(t_control(ConditionNode,ProcessNode,_),EnvIn,EnvOut):-evalCond(ConditionNode,Output,EnvIn,EnvIn),
                                                                  Output = true,
                                                                  evalProcess(ProcessNode,EnvIn,EnvOut).

evalControl(t_control(ConditionNode,_,ProcessNode),EnvIn,EnvOut):-evalCond(ConditionNode,Output,EnvIn,EnvIn),
                                                                  Output = false,!,
                                                                  evalProcess(ProcessNode,EnvIn,EnvOut).

% Rules to evaluate looping construct.
evalIterate(t_iterate(ConditionNode,ProcessNode),EnvIn,EnvOut):- (evalCond(ConditionNode,Output,EnvIn, EnvIn),Output=true ->  
                                                                 evalProcess(ProcessNode, EnvIn, EnvIn2),
                                                                 evalIterate(t_iterate(ConditionNode,ProcessNode), EnvIn2,EnvOut)).
evalIterate(t_iterate(ConditionNode,_),EnvIn,EnvOut):- evalCond(ConditionNode,Output,EnvIn, EnvIn),
                                                       Output=false,!, EnvOut = EnvIn.

%Rules to evaluate condition statements.
evalCond(t_cond(ExpressionNode),Output,EnvIn,EnvIn):-evalBoolexp(ExpressionNode,Output,EnvIn,EnvIn),!.
evalCond(t_cond_not(ExpressionNode),Output,EnvIn,EnvIn):-evalBoolexp(ExpressionNode,BoolOutput,EnvIn,EnvIn),
                                                         BoolOutput = true,
                                                         Output = false,!.

evalCond( t_cond_not(ExpressionNode),Output,EnvIn,EnvIn):-
evalBoolexp(ExpressionNode,BoolOutput,EnvIn,EnvIn),
BoolOutput = false,
Output = true,!.

evalCond(t_cond_and(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
                                                                         evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
                                                                         BoolOutput1 = true,
                                                                         BoolOutput2 = true,
                                                                         Output = true; Output = false,!.

evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):- evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
                                                                         evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
                                                                         BoolOutput1 = true,
                                                                         BoolOutput2 = false,
                                                                         Output = true,!.

evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
                                                                         evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
                                                                         BoolOutput2 = true,
                                                                         BoolOutput1 = false,
                                                                         Output = true,!.

evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
                                                                         evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
                                                                         BoolOutput1 = true,
                                                                         BoolOutput2 = true,
                                                                         Output = true,!.

evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
                                                                         evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
                                                                         BoolOutput1 = false,
                                                                         BoolOutput2 = false,
                                                                         Output = false,!.

%Rules to evaluate boolean expressions.
evalBoolexp(t_boolexp(yes),true,EnvIn,EnvIn).
evalBoolexp(t_boolexp(no),false,EnvIn,EnvIn).

evalBoolexp(t_boolexp_eq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                              evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                              Output1 =:= Output2, Output = true; Output = false,!.

evalBoolexp(t_boolexp_neq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                               evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                               Output1 =\= Output2, Output = true; Output = false,!.

evalBoolexp(t_boolexp_geq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                               evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                               Output1 >= Output2, Output = true; Output = false,!.

evalBoolexp(t_boolexp_leq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                               evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                               Output1 =< Output2, Output = true; Output = false,!.

evalBoolexp(t_boolexp_less(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                               evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                               Output1 < Output2, Output = true; Output = false,!.

evalBoolexp(t_boolexp_great(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                               evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                               Output1 > Output2, Output = true; Output = false,!.

evalBoolexp(t_boolexp_beq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                               evalBoolexp(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                               Output1 =:= Output2, Output = true; Output = false,!.

evalBoolexp(t_boolexp_bneq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
                                                                               evalBoolexp(ExpressionNode1,Output2,EnvIn,EnvIn),
                                                                               Output1 =\= Output2, Output = true; Output = false,!.

% Rules to evaluate expressions.
evalExpression(t_expr(TermNode),Output,EnvIn,EnvIn):- evalTerm(TermNode,Output,EnvIn,EnvIn).


evalExpression(t_add(TermNode,ExpressionNode),Output,EnvIn,EnvIn):- evalTerm(TermNode,Output1,EnvIn,EnvIn),
                                                                    evalExpression(ExpressionNode,Output2,EnvIn,EnvIn),
                                                                    Output is Output1 + Output2,!.

evalExpression(t_sub(TermNode,ExpressionNode),Output,EnvIn,EnvIn):- evalTerm(TermNode,Output1,EnvIn,EnvIn),
                                                                    evalExpression(ExpressionNode,Output2,EnvIn,EnvIn),
                                                                    Output is Output1 - Output2,!.

% Rules to evaluate terms.
evalTerm(t_term(IdentNumNode),Output,EnvIn,EnvIn):-evalNum(IdentNumNode,Output,EnvIn,EnvIn);
                                                   evalNumneg(IdentNumNode,Output,EnvIn,EnvIn),!.

evalTerm(t_term(IdentNumNode),Output,EnvIn,EnvIn):-evalIdentifier(IdentNumNode,Output,EnvIn,EnvIn,_).

evalTerm(t_mul(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalIdentifier(IdentNumNode,Output1,EnvIn,EnvIn,_),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is Output1 * Output2,!.

evalTerm(t_div(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalIdentifier(IdentNumNode,Output1,EnvIn,EnvIn,_),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is Output1 / Output2,!.

evalTerm(t_mod(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalIdentifier(IdentNumNode,Output1,EnvIn,EnvIn,_),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is mod(Output1,Output2),!.

evalTerm(t_mul(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalNum(IdentNumNode,Output1,EnvIn,EnvIn),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is Output1 * Output2,!.

evalTerm(t_div(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalNum(IdentNumNode,Output1,EnvIn,EnvIn),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is Output1 / Output2,!.

evalTerm(t_mod(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalNum(IdentNumNode,Output1,EnvIn,EnvIn),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is mod(Output1,Output2),!.

evalTerm(t_mul(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalNumneg(IdentNumNode,Output1,EnvIn,EnvIn),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is Output1 * Output2,!.

evalTerm(t_div(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalNumneg(IdentNumNode,Output1,EnvIn,EnvIn),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is Output1 / Output2,!.

evalTerm(t_mod(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-evalNumneg(IdentNumNode,Output1,EnvIn,EnvIn),
                                                           evalTerm(TermNode,Output2,EnvIn,EnvIn),
                                                           Output is mod(Output1,Output2),!.

%number, negative number and identifier
evalNum(t_numb(NumNode),Output,EnvIn,EnvIn):-Output is NumNode,!.
evalIdentifier(t_identifier(IdentifierNode),Output,EnvIn,EnvIn,IdentName):-lookup(IdentifierNode, EnvIn, Output),!, IdentName = IdentifierNode.
evalNumneg(t_numbneg(NumNode),Output,EnvIn,EnvIn):-evalNum(NumNode,Output1,EnvIn,EnvIn),Output is 0-Output1,!.