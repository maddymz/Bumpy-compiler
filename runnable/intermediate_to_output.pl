:- style_check(-singleton).

bumpy(FileName) :- open(FileName, read, InStream),
 					read(InStream, InputData),
		      		close(InStream),
		      		evalParser(InputData, EnvOut).

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
	update(Key,Value,[(Key,_)|Tail],[(Key,Value)|T]).
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
	evalAssign(t_assign(IdentifierNode,ExpressionNode),EnvIn,EnvOut):-
	evalIdentifier(IdentifierNode,_,EnvIn,EnvIn,IdentName),evalExpression(ExpressionNode,Output,EnvIn,EnvIn),
	update(IdentName,Output, EnvIn,EnvOut),!.

	evalAssign(t_assign(IdentifierNode,ExpressionNode),EnvIn,EnvOut):-
	evalIdentifier(IdentifierNode,_,EnvIn,EnvIn,IdentName),evalBoolexp(ExpressionNode,Output,EnvIn,EnvIn),
	update(IdentName,Output, EnvIn,EnvOut),!.

	%Rules to evaluate control statements.
	evalControl(t_control(ConditionNode,ProcessNode,_),EnvIn,EnvOut):-
	evalCond(ConditionNode,Output,EnvIn,EnvIn),
	Output = true,
	evalProcess(ProcessNode,EnvIn,EnvOut).

	evalControl(t_control(ConditionNode,_,ProcessNode),EnvIn,EnvOut):-
	evalCond(ConditionNode,Output,EnvIn,EnvIn),
	Output = false,!,
	evalProcess(ProcessNode,EnvIn,EnvOut).

	% Rules to evaluate looping construct.
	evalIterate(t_iterate(ConditionNode,ProcessNode),EnvIn,EnvOut):- (evalCond(ConditionNode,Output,EnvIn, EnvIn),Output=true ->  evalProcess(ProcessNode, EnvIn, EnvIn2),evalIterate(t_iterate(ConditionNode,ProcessNode), EnvIn2,EnvOut)).
	evalIterate(t_iterate(ConditionNode,_),EnvIn,EnvOut):- evalCond(ConditionNode,Output,EnvIn, EnvIn),Output=false,!, EnvOut = EnvIn.

	%Rules to evaluate condition statements.
	evalCond(t_cond(ExpressionNode),Output,EnvIn,EnvIn):-evalBoolexp(ExpressionNode,Output,EnvIn,EnvIn),!.
	evalCond(t_cond_not(ExpressionNode),Output,EnvIn,EnvIn):-
	evalBoolexp(ExpressionNode,BoolOutput,EnvIn,EnvIn),
	BoolOutput = true,
	Output = false,!.

	evalCond(t_cond_not(ExpressionNode),Output,EnvIn,EnvIn):-
	evalBoolexp(ExpressionNode,BoolOutput,EnvIn,EnvIn),
	BoolOutput = false,
	Output = true,!.

	evalCond(t_cond_and(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = true,
	BoolOutput2 = true,
	Output = true; Output = false,!.

	evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = true,
	BoolOutput2 = false,
	Output = true,!.

	evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
	BoolOutput2 = true,
	BoolOutput1 = false,
	Output = true,!.

	evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = true,
	BoolOutput2 = true,
	Output = true,!.

	evalCond(t_cond_or(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalBoolexp(ExpressionNode,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(ExpressionNode1,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = false,
	BoolOutput2 = false,
	Output = false,!.

	%Rules to evaluate boolean expressions.
	evalBoolexp(t_boolexp(yes),true,EnvIn,EnvIn).
	evalBoolexp(t_boolexp(no),false,EnvIn,EnvIn).
	evalBoolexp(t_boolexp_eq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 =:= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_neq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 =\= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_geq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 >= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_leq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 =< Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_less(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 < Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_great(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 > Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_beq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalBoolexp(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 =:= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_bneq(ExpressionNode,ExpressionNode1),Output,EnvIn,EnvIn):-
	evalExpression(ExpressionNode,Output1,EnvIn,EnvIn),
	evalBoolexp(ExpressionNode1,Output2,EnvIn,EnvIn),
	Output1 =\= Output2, Output = true; Output = false,!.

	% Rules to evaluate expressions.
	evalExpression(t_expr(TermNode),Output,EnvIn,EnvIn):-
	evalTerm(TermNode,Output,EnvIn,EnvIn).


	evalExpression(t_add(TermNode,ExpressionNode),Output,EnvIn,EnvIn):- 
	evalTerm(TermNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode,Output2,EnvIn,EnvIn),
	Output is Output1 + Output2,!.

	evalExpression(t_sub(TermNode,ExpressionNode),Output,EnvIn,EnvIn):- 
	evalTerm(TermNode,Output1,EnvIn,EnvIn),
	evalExpression(ExpressionNode,Output2,EnvIn,EnvIn),
	Output is Output1 - Output2,!.

	% Rules to evaluate terms.
	evalTerm(t_term(IdentNumNode),Output,EnvIn,EnvIn):-
	evalNum(IdentNumNode,Output,EnvIn,EnvIn);
	evalNumneg(IdentNumNode,Output,EnvIn,EnvIn),!.

	evalTerm(t_term(IdentNumNode),Output,EnvIn,EnvIn):-evalIdentifier(IdentNumNode,Output,EnvIn,EnvIn,_).

	evalTerm(t_mul(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalIdentifier(IdentNumNode,Output1,EnvIn,EnvIn,_),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is Output1 * Output2,!.

	evalTerm(t_div(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalIdentifier(IdentNumNode,Output1,EnvIn,EnvIn,_),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is Output1 / Output2,!.

	evalTerm(t_mod(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalIdentifier(IdentNumNode,Output1,EnvIn,EnvIn,_),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is mod(Output1,Output2),!.

	evalTerm(t_mul(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalNum(IdentNumNode,Output1,EnvIn,EnvIn),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is Output1 * Output2,!.

	evalTerm(t_div(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalNum(IdentNumNode,Output1,EnvIn,EnvIn),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is Output1 / Output2,!.

	evalTerm(t_mod(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalNum(IdentNumNode,Output1,EnvIn,EnvIn),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is mod(Output1,Output2),!.

	evalTerm(t_mul(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalNumneg(IdentNumNode,Output1,EnvIn,EnvIn),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is Output1 * Output2,!.

	evalTerm(t_div(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalNumneg(IdentNumNode,Output1,EnvIn,EnvIn),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is Output1 / Output2,!.

	evalTerm(t_mod(IdentNumNode,TermNode),Output,EnvIn,EnvIn):-
	evalNumneg(IdentNumNode,Output1,EnvIn,EnvIn),
	evalTerm(TermNode,Output2,EnvIn,EnvIn),
	Output is mod(Output1,Output2),!.

	%number, negative number and identifier
	evalNum(t_numb(NumNode),Output,EnvIn,EnvIn):-Output is NumNode,!.
	evalIdentifier(t_identifier(IdentifierNode),Output,EnvIn,EnvIn,IdentName):-lookup(IdentifierNode, EnvIn, Output),!, IdentName = IdentifierNode.
	evalNumneg(t_numbneg(NumNode),Output,EnvIn,EnvIn):-evalNum(NumNode,Output1,EnvIn,EnvIn),Output is 0-Output1,!.