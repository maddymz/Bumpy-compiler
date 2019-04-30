:- style_check(-singleton).

bumpy(FileName) :- open(FileName, read, InStream),
 					read(InStream, X),
		      		close(InStream),
		      		evalParser(X, EnvOut).

	evalParser(X, EnvOut) :- evalProgram(X,[],EnvOut).

	evalProgram(t_program(_,Y),EnvIn, EnvOut) :-evalBlock(Y,EnvIn, EnvOut),!.
	evalProgram(t_program(X),EnvIn, EnvOut) :- evalBlock(X,EnvIn, EnvOut),!.

	evalProcess(t_process(X),EnvIn,EnvOut):-evalPrint(X,EnvIn, EnvOut),!.
	evalProcess(t_process(X),EnvIn,EnvOut):-evalAssign(X,EnvIn,EnvOut),!.
	evalProcess(t_process(X),EnvIn,EnvOut):-evalIterate(X,EnvIn,EnvOut),!.
	evalProcess(t_process(X),EnvIn,EnvOut):-evalControl(X,EnvIn,EnvOut),!.
	evalProcess(t_process(X),EnvIn,EnvOut):-evalRead(X,EnvIn,EnvOut),!.
	evalProcess(t_process(X,Y),EnvIn,EnvOut):-evalAssign(X,EnvIn,EnvIn2),
	evalProcess(Y,EnvIn2,EnvOut),!.
	evalProcess(t_process(X,Y),EnvIn,EnvOut):-evalIterate(X,EnvIn,EnvIn2),
	evalProcess(Y,EnvIn2,EnvOut),!.
	evalProcess(t_process(X,Y),EnvIn,EnvOut):-evalControl(X,EnvIn,EnvIn2),
	evalProcess(Y,EnvIn2,EnvOut),!.
	evalProcess(t_process(X,Y), EnvIn, EnvOut) :- evalPrint(X,EnvIn,EnvIn2),
	evalProcess(Y,EnvIn2, EnvOut),!.
	evalProcess(t_process(X,Y), EnvIn, EnvOut) :- evalRead(X, EnvIn, EnvIn2),
	evalProcess(Y,EnvIn2,EnvOut).

	% Read statement
	evalRead(t_read(X), EnvIn, EnvOut):- read(Term), evalIdentifier(X,_,EnvIn,EnvIn2,IdentName), 
										update(IdentName, Term, EnvIn2, EnvOut).

	% Look up the environment to find the value of a variable
	lookup(_,[],0).
	lookup(X,[(X, V)|_],V).
	lookup(X,[_|T],V) :- lookup(X,T,V).

	% Update the env with new values of variables
	update(X,V,[],[(X,V)]).
	update(X,V,[(X,_)|T],[(X,V)|T]).
	update(X,V,[H|T],[H|T1]) :- update(X,V,T,T1).

	% iterate
	evalIterate(t_iterate(X,Y),EnvIn,EnvOut):- (evalCond(X,Output,EnvIn, EnvIn),Output=true ->  evalProcess(Y, EnvIn, EnvIn2),evalIterate(t_iterate(X,Y), EnvIn2,EnvOut)).
	evalIterate(t_iterate(X,_),EnvIn,EnvOut):- evalCond(X,Output,EnvIn, EnvIn),Output=false,!, EnvOut = EnvIn.

	% Rules to evaluate print statements.
	evalPrint(t_print(X),EnvIn,EnvOut) :-  evalExpression(X,Output,EnvIn,EnvOut), write(Output).
	evalPrint(t_printString(X), EnvIn, EnvIn) :- write(X), write(" "),!.

	%Control
	evalControl(t_control(X,Y,_),EnvIn,EnvOut):-
	evalCond(X,Output,EnvIn,EnvIn),
	Output = true,
	evalProcess(Y,EnvIn,EnvOut).

	evalControl(t_control(X,_,Z),EnvIn,EnvOut):-
	evalCond(X,Output,EnvIn,EnvIn),
	Output = false,!,
	evalProcess(Z,EnvIn,EnvOut).

	%Condition
	evalCond(t_cond(X),Output,EnvIn,EnvIn):-evalBoolexp(X,Output,EnvIn,EnvIn),!.
	evalCond(t_cond_not(X),Output,EnvIn,EnvIn):-
	evalBoolexp(X,BoolOutput,EnvIn,EnvIn),
	BoolOutput = true,
	Output = false,!.

	evalCond(t_cond_not(X),Output,EnvIn,EnvIn):-
	evalBoolexp(X,BoolOutput,EnvIn,EnvIn),
	BoolOutput = false,
	Output = true,!.

	evalCond(t_cond_and(X,Y),Output,EnvIn,EnvIn):-
	evalBoolexp(X,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(Y,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = true,
	BoolOutput2 = true,
	Output = true; Output = false,!.

	evalCond(t_cond_or(X,Y),Output,EnvIn,EnvIn):-
	evalBoolexp(X,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(Y,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = true,
	BoolOutput2 = false,
	Output = true,!.

	evalCond(t_cond_or(X,Y),Output,EnvIn,EnvIn):-
	evalBoolexp(X,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(Y,BoolOutput2,EnvIn,EnvIn),
	BoolOutput2 = true,
	BoolOutput1 = false,
	Output = true,!.

	evalCond(t_cond_or(X,Y),Output,EnvIn,EnvIn):-
	evalBoolexp(X,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(Y,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = true,
	BoolOutput2 = true,
	Output = true,!.

	evalCond(t_cond_or(X,Y),Output,EnvIn,EnvIn):-
	evalBoolexp(X,BoolOutput1,EnvIn,EnvIn),
	evalBoolexp(Y,BoolOutput2,EnvIn,EnvIn),
	BoolOutput1 = false,
	BoolOutput2 = false,
	Output = false,!.

					

	%Block
	evalBlock(t_block(X,Y),EnvIn,EnvOut) :-
	evalDeclaration(X,EnvIn,EnvIn2),
	evalProcess(Y,EnvIn2,EnvOut).

	%datatype
	evalDatatype(t_datatype(var),Output,EnvIn,EnvIn):-Output=var,!.
	evalDatatype(t_datatype(bool),Output,EnvIn,EnvIn):-Output=bool,!.

	%bool expression
	evalBoolexp(t_boolexp(yes),true,EnvIn,EnvIn).
	evalBoolexp(t_boolexp(no),false,EnvIn,EnvIn).
	evalBoolexp(t_boolexp_eq(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output1 =:= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_neq(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output1 =\= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_geq(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output1 >= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_leq(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output1 =< Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_less(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output1 < Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_great(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output1 > Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_beq(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalBoolexp(Y,Output2,EnvIn,EnvIn),
	Output1 =:= Output2, Output = true; Output = false,!.

	evalBoolexp(t_boolexp_bneq(X,Y),Output,EnvIn,EnvIn):-
	evalExpression(X,Output1,EnvIn,EnvIn),
	evalBoolexp(Y,Output2,EnvIn,EnvIn),
	Output1 =\= Output2, Output = true; Output = false,!.


	%declaration
	evalDeclaration(t_declare(X,Y),EnvIn,EnvOut) :-
	evalDatatype(X,_,EnvIn,EnvIn),
	evalIdentifier(Y,_,EnvIn,EnvIn,IdentName),update(IdentName,0, EnvIn, EnvOut).

	evalDeclaration(t_declare(X,Y,Z),EnvIn,EnvOut) :-
	evalDatatype(X,_,EnvIn,EnvIn),
	evalIdentifier(Y,_,EnvIn,EnvIn,IdentName),update(IdentName,0, EnvIn, EnvIn2),
	evalDeclaration(Z,EnvIn2,EnvOut).

	% assignment
	evalAssign(t_assign(X,Y),EnvIn,EnvOut):-
	evalIdentifier(X,_,EnvIn,EnvIn,IdentName),evalExpression(Y,Output,EnvIn,EnvIn),
	update(IdentName,Output, EnvIn,EnvOut),!.

	evalAssign(t_assign(X,Y),EnvIn,EnvOut):-
	evalIdentifier(X,_,EnvIn,EnvIn,IdentName),evalBoolexp(Y,Output,EnvIn,EnvIn),
	update(IdentName,Output, EnvIn,EnvOut),!.

	%expressions
	evalExpression(t_expr(X),Output,EnvIn,EnvIn):-
	evalTerm(X,Output,EnvIn,EnvIn).


	evalExpression(t_add(X,Y),Output,EnvIn,EnvIn):- 
	evalTerm(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output is Output1 + Output2,!.

	evalExpression(t_sub(X,Y),Output,EnvIn,EnvIn):- 
	evalTerm(X,Output1,EnvIn,EnvIn),
	evalExpression(Y,Output2,EnvIn,EnvIn),
	Output is Output1 - Output2,!.

	%terms
	evalTerm(t_term(X),Output,EnvIn,EnvIn):-
	evalNum(X,Output,EnvIn,EnvIn);
	evalNumneg(X,Output,EnvIn,EnvIn),!.

	evalTerm(t_term(X),Output,EnvIn,EnvIn):-evalIdentifier(X,Output,EnvIn,EnvIn,_).

	evalTerm(t_mul(X,Y),Output,EnvIn,EnvIn):-
	evalIdentifier(X,Output1,EnvIn,EnvIn,_),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is Output1 * Output2,!.

	evalTerm(t_div(X,Y),Output,EnvIn,EnvIn):-
	evalIdentifier(X,Output1,EnvIn,EnvIn,_),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is Output1 / Output2,!.

	evalTerm(t_mod(X,Y),Output,EnvIn,EnvIn):-
	evalIdentifier(X,Output1,EnvIn,EnvIn,_),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is mod(Output1,Output2),!.

	evalTerm(t_mul(X,Y),Output,EnvIn,EnvIn):-
	evalNum(X,Output1,EnvIn,EnvIn),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is Output1 * Output2,!.

	evalTerm(t_div(X,Y),Output,EnvIn,EnvIn):-
	evalNum(X,Output1,EnvIn,EnvIn),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is Output1 / Output2,!.

	evalTerm(t_mod(X,Y),Output,EnvIn,EnvIn):-
	evalNum(X,Output1,EnvIn,EnvIn),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is mod(Output1,Output2),!.

	evalTerm(t_mul(X,Y),Output,EnvIn,EnvIn):-
	evalNumneg(X,Output1,EnvIn,EnvIn),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is Output1 * Output2,!.

	evalTerm(t_div(X,Y),Output,EnvIn,EnvIn):-
	evalNumneg(X,Output1,EnvIn,EnvIn),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is Output1 / Output2,!.

	evalTerm(t_mod(X,Y),Output,EnvIn,EnvIn):-
	evalNumneg(X,Output1,EnvIn,EnvIn),
	evalTerm(Y,Output2,EnvIn,EnvIn),
	Output is mod(Output1,Output2),!.

	%number , neg number and identifier done
	evalNum(t_numb(X),Output,EnvIn,EnvIn):-Output is X,!.
	evalIdentifier(t_identifier(X),Output,EnvIn,EnvIn,IdentName):-lookup(X, EnvIn, Output),!, IdentName = X.
	evalNumneg(t_numbneg(X),Output,EnvIn,EnvIn):-evalNum(X,Output1,EnvIn,EnvIn),Output is 0-Output1,!.