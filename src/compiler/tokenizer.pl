tokenizer(S,L) :-
    split_string(S," "," ", L1),
    atomize(L1,L).

atomize([],[]).
atomize([H|T],[H1|T1]):-
    atom_string(H1,H),
    atomize(T,T1).