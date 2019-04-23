bumpy(FileName) :- open(FileName, read, InStream),
 					read(InStream, X),
		      		close(InStream),
		      		Env = [],
		      		evalParser(X, Env, EnvOut).

evalParser(X, EnvIn, EnvOut) :- evalProgram(X, EnvIn, EnvOut).

% Look up the environment to find the value of a variable
lookup(_,[],0).
lookup(X,[(X, V)|_],V).
lookup(X,[_|T],V) :- lookup(X,T,V).

% Update the env with new values of variables
update(X,V,[],[(X,V)]).
update(X,V,[(X,_)|T],[(X,V)|T]).
update(X,V,[H|T],[H|T1]) :- update(X,V,T,T1).

evalProgram(t_program(X, Y), EnvIn, EnvOut) :-evalComment(X, EnvIn, EnvIn2),evalBlock(Y, EnvIn2, EnvOut),!.
evalProgram(t_program(X), EnvIn, EnvOut) :- evalBlock(X, E0, E).

evalComment(t_comment(X),EnvIn,EnvOut) :-evalWords(X,E0,E).

evalWords(t_words(X,Y),EnvIn,EnvOut) :-evalIdentifier(X,EnvIn,EnvIn2),evalWords(Y,EnvIn2,EnvOut),!.



% Rules for declaration.
evalDeclaration(t_declare(X, Y, Z), EnvIn, EnvOut) :- evalDatatype(X, EnvIn, EnvIn1), evalIdentifier(Y, _, IdName, EnvIn1, EnvIn2), 
evalDeclaration(Z, EnvIn2, EnvIn3), update(IdName, 0, EnvIn3, EnvOut).
evalDeclaration(t_declare(X, Y), EnvIn, EnvOut) :- evalDatatype(X, EnvIn, EnvIn1),
evalIdentifier(Y, _, IdName, EnvIn1, EnvIn2), update(IdName, 0, EnvIn2, EnvOut).

evalAssign(t_assign(X,Y), EnvIn, EnvOut) :- 
    evalIdentifier(X, _, IdName, EnvIn, EnvIn2),
    evalExpression(Y, Output, EnvIn2, EnvIn3),
    update(IdName, Output, EnvIn3, EnvOut).

evalAssign(t_assign(X,Y), EnvIn, EnvOut) :- 
    evalIdentifier(X, _, IdName, EnvIn, EnvIn2),
    evalBoolExpression(Y, Output, EnvIn2, EnvIn3),
    update(IdName, Output, EnvIn3, EnvOut).

evalNum(t_num(X), Output, EnvIn, EnvIn) :- Output = X, !.
evalNumneg(t_numneg(X), Output, EnvIn, EnvIn) :- Output = X, !.
evalIdentifier(t_identifier(X), Output, Ident, EnvIn, EnvIn) :- lookup(X, EnvIn, Output), Ident = X.

evalDatatype(t_datatype(var), EnvIn, EnvIn).
evalDatatype(t_datatype(bool), EnvIn, EnvIn).

% Rules to evaluate expressions.

evalExpression(t_add(X,Y), Output, EnvIn, EnvOut) :- evalTerm(X, TermOut, EnvIn, EnvIn2),
                                                     evalExpression(Y, ExpOut, EnvIn2, EnvOut),
                             atom_string(TermOut, QtermOut),
                             atom_number(QtermOut, NtermOut),
                             atom_string(ExpOut, QexpOut),
                             atom_number(QexpOut, NexpOut),
                                                     Output is NtermOut + NexpOut.

evalExpression(t_sub(X,Y), Output, EnvIn, EnvOut) :- evalTerm(X, TermOut, EnvIn, EnvIn2),
                                                     evalExpression(Y, ExpOut, EnvIn2, EnvOut),
                             atom_string(TermOut, QtermOut),
                             atom_number(QtermOut, NtermOut),
                             atom_string(ExpOut, QexpOut),
                             atom_number(QexpOut, NexpOut),
                                                     Output is NtermOut - NexpOut.

evalExpression(t_exp(X), Output, EnvIn, EnvOut) :- evalTerm(X, Output, EnvIn, EnvOut).

%rule for term
evalTerm(t_mul(X,Y), Output, EnvIn, EnvOut) :- evalFactor(X, FactOut, EnvIn, EnvIn2),
                                               evalTerm(Y, TermOut, EnvIn2, EnvOut),
                           atom_string(TermOut, QtermOut),
                           atom_number(QtermOut, NtermOut),
                           atom_string(FactOut, QfactOut),
                           atom_number(QfactOut, NfactOut),
                                               Output is NfactOut * NtermOut.

evalTerm(t_div(X,Y), Output, EnvIn, EnvOut) :- evalFactor(X, FactOut, EnvIn, EnvIn2),
                                               evalTerm(Y, TermOut, EnvIn2, EnvOut),
                           atom_string(TermOut, QtermOut),
                            atom_number(QtermOut, NtermOut),
                            atom_string(FactOut, QfactOut),
                            atom_number(QfactOut, NfactOut),
                                               Output is NfactOut / NtermOut.

evalTerm(t_mod(X,Y), Output, EnvIn, EnvOut) :- evalFactor(X, FactOut, EnvIn, EnvIn2),
                                               evalTerm(Y, TermOut, EnvIn2, EnvOut),
                           atom_string(TermOut, QtermOut),
                            atom_number(QtermOut, NtermOut),
                            atom_string(FactOut, QfactOut),
                            atom_number(QfactOut, NfactOut),
                                               Output is NfactOut mod NtermOut.

evalTerm(t_term(X), Output, EnvIn, EnvOut) :- evalNum(X, Output, EnvIn, EnvOut).
evalTerm(t_term(X), Output, EnvIn, EnvOut) :- evalNumneg(X, Output, EnvIn, EnvOut).
evalTerm(t_term(X), Output, EnvIn, EnvOut) :- evalIdentifier(X, Output, EnvIn, EnvOut).

%rule for boolexpression
evalBoolexp(t_boolexp_eq(X,Y), Output, EnvIn, EnvOut) :- evalExpression(X, ExpOutput1, _, EnvIn, EnvIn2)
                                                       	evalExpression(Y, ExpOutput2, EnvIn2, EnvOut),
						       							atom_string(ExpOutput1, Qstring),
						       							atom_number(Qstring, NExp1),
						       							atom_string(ExpOutput2, QExp2),
						       							atom_number(QExp2, NExp2),
														((NExp1 =:= NExp2) -> !; !,false).


evalBoolexp(t_boolexp_neq(X,Y), Output, EnvIn, EnvOut) :- evalIdentifier(X, ExpOutput1, _, EnvIn, EnvIn2)
                                                       	evalExpression(Y, ExpOutput2, EnvIn2, EnvOut),
						       							atom_string(ExpOutput1, Qstring),
						       							atom_number(Qstring, NExp1),
						       							atom_string(ExpOutput2, QExp2),
						       							atom_number(QExp2, NExp2),
														((NExp1 \= NExp2) -> !; !,false).

evalBoolexp(t_boolexp_leq(X,Y), Output, EnvIn, EnvOut) :- evalIdentifier(X, ExpOutput1, _, EnvIn, EnvIn2)
                                                       	evalExpression(Y, ExpOutput2, EnvIn2, EnvOut),
						       							atom_string(ExpOutput1, Qstring),
						       							atom_number(Qstring, NExp1),
						       							atom_string(ExpOutput2, QExp2),
						       							atom_number(QExp2, NExp2),
														((NExp1 <= NExp2) -> !; !,false).

evalBoolexp(t_boolexp_geq(X,Y), Output, EnvIn, EnvOut) :- evalIdentifier(X, ExpOutput1, _, EnvIn, EnvIn2)
                                                       	evalExpression(Y, ExpOutput2, EnvIn2, EnvOut),
						       							atom_string(ExpOutput1, Qstring),
						       							atom_number(Qstring, NExp1),
						       							atom_string(ExpOutput2, QExp2),
						       							atom_number(QExp2, NExp2),
														((NExp1 >= NExp2) -> !; !,false).

evalBoolexp(t_boolexp_less(X,Y), Output, EnvIn, EnvOut) :- evalIdentifier(X, ExpOutput1, _, EnvIn, EnvIn2)
                                                       	evalExpression(Y, ExpOutput2, EnvIn2, EnvOut),
						       							atom_string(ExpOutput1, Qstring),
						       							atom_number(Qstring, NExp1),
						       							atom_string(ExpOutput2, QExp2),
						       							atom_number(QExp2, NExp2),
														((NExp1 < NExp2) -> !; !,false).

evalBoolexp(t_boolexp_great(X,Y), Output, EnvIn, EnvOut) :- evalIdentifier(X, ExpOutput1, _, EnvIn, EnvIn2)
                                                       	evalExpression(Y, ExpOutput2, EnvIn2, EnvOut),
						       							atom_string(ExpOutput1, Qstring),
						       							atom_number(Qstring, NExp1),
						       							atom_string(ExpOutput2, QExp2),
						       							atom_number(QExp2, NExp2),
														((NExp1 > NExp2) -> !; !,false).

evalBoolexp(t_boolexp_beq(X,Y), Output, EnvIn, EnvOut) :- evalBoolexp(Y, BoolExpOutput1, EnvIn2, EnvOut),
                                                       	evalBoolexp(Y, BoolExpOutput2, EnvIn2, EnvOut),
						       							atom_string(BoolExpOutput1, Qstring),
						       							atom_number(Qstring, NBExp1),
						       							atom_string(BoolExpOutput2, QExp2),
						       							atom_number(QExp2, NBExp2),
														((NBExp1 =:= NBExp2) -> !; !,false).

evalBoolexp(t_boolexp_bneq(X,Y), Output, EnvIn, EnvOut) :- evalBoolexp(Y, BoolExpOutput1, EnvIn2, EnvOut),
                                                       	evalBoolexp(Y, BoolExpOutput2, EnvIn2, EnvOut),
						       							atom_string(BoolExpOutput1, Qstring),
						       							atom_number(Qstring, NBExp1),
						       							atom_string(BoolExpOutput2, QExp2),
						       							atom_number(QExp2, NBExp2),
														((NBExp1 \= NBExp2) -> !; !,false).


evalBoolexp(t_boolexp(true), EnvIn, EnvIn) :- true.
evalBoolexp(t_boolexp(false), EnvIn, EnvIn) :- false.

% rule for control
evalControl(t_control(X,Y,Z), EnvIn,EnvOut):- (evalCondition(X, EnvIn, EnvIn1)->  evalProcess(Y, EnvIn1, EnvOut));
                    evalProcess(Z,EnvIn,EnvOut).

%rule for condition 
evalCondition(t_cond_and(X,Y),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvIn1),evalBoolexp(Y, Output, EnvIn1, EnvOut).
evalCondition(t_cond_or(X,Y),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvIn1),evalBoolexp(Y, Output, EnvIn1, EnvOut).
evalCondtion(t_cond_not(X),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvOut).
evalCondition(t_cond(X),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvOut).