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

evalBoolexp(t_boolexp(X,Y), Output, EnvIn, EnvOut) :- evalIdentifier(X, IdOutput, _, EnvIn, EnvIn2)
                                                       evalExpression(Y, ExpOutput, EnvIn2, EnvOut),
						       							atom_string(IdOutput, Qstring),
						       							atom_number(Qstring, NIdOut),
						       							atom_string(ExpOutput, QExp),
						       							atom_number(QExp, NExp),
														((NIdOut =:= NExp) -> !; !,false).