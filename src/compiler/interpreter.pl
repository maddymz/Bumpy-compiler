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
evalTerm(t_term(X), Output, EnvIn, EnvOut) :- evalFactor(X, Output, EnvIn, EnvOut).

evalFactor(t_num(X), Output, EnvIn, EnvOut) :- evalNum(X, Output, EnvIn, EnvOut).
evalFactor(t_numneg(X), Output, EnvIn, EnvOut) :- evalNumneg(X, Output, EnvIn, EnvOut).
evalFactor(t_identifier(X), Output, EnvIn, EnvOut) :- evalIdentifier(X, Output, EnvIn, EnvOut).

evalNum(t_num(X), Output, EnvIn, EnvIn) :- Output = X, !.
evalNumneg(t_numneg(X), Output, EnvIn, EnvIn) :- Output = X, !.
evalIdentifier(t_identifier(X), Output, Ident, EnvIn, EnvIn) :- lookup(X, EnvIn, Output), Ident = X.

evalDatatype(t_datatype(var), EnvIn, EnvIn).
evalDatatype(t_datatype(bool), EnvIn, EnvIn).
