evalProgram(t_program(X, Y), EnvIn, EnvOut) :-evalComment(X, EnvIn, EnvIn2),evalBlock(Y, EnvIn2, EnvOut),!.
evalProgram(t_program(X), EnvIn, EnvOut) :- evalBlock(X, EnvIn, EnvOut).

evalComment(t_comment(X),EnvIn,EnvOut) :-evalWords(X,EnvIn,EnvOut).

evalWords(t_words(X,Y),EnvIn,EnvOut) :-evalIdentifier(X,EnvIn,EnvIn2),evalWords(Y,EnvIn2,EnvOut),!.

evalBlock(t_block(X,Y),EnvIn,EnvOut) :-evalDeclaration(X,EnvIn, EnvIn2),evalProcess(Y,EnvIn2,EnvOut),!.

evalProcess(t_process(X,Y),EnvIn,EnvOut):-evalAssign(X,EnvIn, EnvIn2),evalProcess(Y,EnvIn2,EnvOut).
evalProcess(t_process(X,Y),EnvIn,EnvOut):-evalControl(X,EnvIn, EnvIn2),evalProcess(Y,EnvIn2,EnvOut).
evalProcess(t_process(X,Y),EnvIn,EnvOut):-evalIterate(X,EnvIn, EnvIn2),evalProcess(Y,EnvIn2,EnvOut).

evalProcess(t_process(X),EnvIn,EnvOut):-evalAssign(X,EnvIn, EnvOut).
evalProcess(t_process(X),EnvIn,EnvOut):-evalControl(X,EnvIn, EnvOut).
evalProcess(t_process(X),EnvIn,EnvOut):-evalIterate(X,EnvIn, EnvOut).


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

evalTerm(t_mul(X,Y), Output, EnvIn, EnvOut) :- evalTerm(X, TermOut, EnvIn, EnvIn2),
                                                     evalExpression(Y, ExpOut, EnvIn2, EnvOut),
                             atom_string(TermOut, QtermOut),
                             atom_number(QtermOut, NtermOut),
                             atom_string(ExpOut, QexpOut),
                             atom_number(QexpOut, NexpOut),
                                               Output is NfactOut * NtermOut.

evalTerm(t_div(X,Y), Output, EnvIn, EnvOut) :- evalTerm(X, TermOut, EnvIn, EnvIn2),
                                                     evalExpression(Y, ExpOut, EnvIn2, EnvOut),
                             atom_string(TermOut, QtermOut),
                             atom_number(QtermOut, NtermOut),
                             atom_string(ExpOut, QexpOut),
                             atom_number(QexpOut, NexpOut),
                                               Output is NfactOut / NtermOut.

evalTerm(t_mod(X,Y), Output, EnvIn, EnvOut) :- evalTerm(X, TermOut, EnvIn, EnvIn2),
                                                     evalExpression(Y, ExpOut, EnvIn2, EnvOut),
                             atom_string(TermOut, QtermOut),
                             atom_number(QtermOut, NtermOut),
                             atom_string(ExpOut, QexpOut),
                             atom_number(QexpOut, NexpOut),
                                               Output is NfactOut mod NtermOut.

evalTerm(t_term(X), Output, EnvIn, EnvOut) :- evalNum(X, Output, EnvIn, EnvOut).
evalTerm(t_term(X), Output, EnvIn, EnvOut) :- evalNumneg(X, Output, EnvIn, EnvOut).
evalTerm(t_term(X), Output, EnvIn, EnvOut) :- evalIdentifier(X, Output, EnvIn, EnvOut).

% rule for control
evalControl(t_control(X,Y,Z), EnvIn,EnvOut):- (evalCondition(X, EnvIn, EnvIn1)->  evalProcess(Y, EnvIn1, EnvOut));
    								evalProcess(Z,EnvIn,EnvOut).

% rule for iterate
evalIterate(t_iterate(X,Y), EnvIn,EnvOut):- (evalCondition(X, EnvIn, EnvIn1)->  evalProcess(Y, EnvIn1, EnvIn2),evalIterate(t_iterate(X,Y), EnvIn2,EnvOut));EnvOut = EnvIn.

%rule for condition 
evalCondition(t_cond_and(X,Y),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvIn1),evalBoolexp(Y, Output, EnvIn1, EnvOut).
evalCondition(t_cond_or(X,Y),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvIn1),evalBoolexp(Y, Output, EnvIn1, EnvOut).
evalCondtion(t_cond_not(X),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvOut).
evalCondition(t_cond(X),Output, EnvIn,EnvOut):- evalBoolexp(X,Output, EnvIn,EnvOut).



