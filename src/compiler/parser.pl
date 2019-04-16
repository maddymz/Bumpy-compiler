program(t_program(X,Y)) -->comment(X),block(Y).
program(t_program(X)) -->block(X).

comment(t_comment(Y)) --> [@],words(Y),[@].

words(t_words(X,Y)) --> identifier(X), words(Y) ; numb(X), words(Y).
words(t_words(X)) -->identifier(X); numb(X).

block(t_block(X,Y)) --> [start], declaration(X), process(Y), [end].

declaration(t_declare(X,Y)) --> [var], identifier(X), [;], declaration(Y) ; [bool], identifier(X), [;] ,declaration(Y).    
declaration(t_declare(X)) -->[var], identifier(X),[;]; [bool], identifier(X),[;].

process(t_process(X,Y)) --> assignvalue(X), [;], process(Y); control(X), process(Y); iterate(X), process(Y).
process(t_process(X)) -->assignvalue(X),[;] ;control(X) ;iterate(X).

assignvalue(t_assign(X,Y)) --> identifier(X), [=] ,expression(Y); identifier(X), [is], boolexp(Y).

control(t_control(X,Y,Z)) --> [incase], condition(X), [do], process(Y), [otherwise], process(Z), [endcase].

iterate(t_iterate(X,Y)) --> [when], condition(X), [repeat], process(Y), [endrepeat].

condition(t_cond(X,Y)) --> boolexp(X), [and], boolexp(Y); boolexp(X), [or], boolexp(Y).
condition(t_cond(X)) -->[~], boolexp(X); boolexp(X).

boolexp(t_boolexp(X,Y)) --> expression(X), [:=:], expression(Y); expression(X), [~=], expression(Y); 
    expression(X), [<=], expression(Y); expression(X), [>=], expression(Y); expression(X), [<], expression(Y);
    expression(X), [>], expression(Y). 
boolexp(t_boolexp(yes)) --> [yes].
boolexp(t_boolexp(no)) --> [no].

expression(t_expr(X,Y)) --> term(X),[+],expression(Y); term(X),[-],expression(Y).
expression(t_expr(X)) --> term(X).

term(t_term(X,Y)) --> identifier(X),[*],term(Y); numb(X),[*],term(Y); identifier(X),[/],term(Y); numb(X),[/],term(Y);
    identifier(X),[mod],term(Y); numb(X),[mod],term(Y).
term(t_term(X)) -->identifier(X); numb(X).

%identifier --> [a],identifier;[b],identifier;[c],identifier;[d],identifier;[e],identifier;[f],identifier;[g],identifier;[h],identifier;[i],identifier;[j],identifier;[k],identifier;[l],identifier;[m],identifier;[n],identifier;[o],identifier;[p],identifier;[q],identifier;[r],identifier;[s],identifier;[t],identifier;[u],identifier;[v],identifier;[w],identifier;[x],identifier;[y],identifier;[z],identifier.
identifier(t_identifier(a)) -->[a].
identifier(t_identifier(b)) -->[b].
identifier(t_identifier(c)) -->[c].
identifier(t_identifier(d)) -->[d].
identifier(t_identifier(e)) -->[e].
identifier(t_identifier(f)) -->[f].
identifier(t_identifier(g)) -->[g].
identifier(t_identifier(h)) -->[h].
identifier(t_identifier(i)) -->[i].
identifier(t_identifier(j)) -->[j].
identifier(t_identifier(k)) -->[k].
identifier(t_identifier(l)) -->[l].
identifier(t_identifier(m)) -->[m].
identifier(t_identifier(n)) -->[n].
identifier(t_identifier(o)) -->[o].
identifier(t_identifier(p)) -->[p].
identifier(t_identifier(q)) -->[q].
identifier(t_identifier(r)) -->[r].
identifier(t_identifier(s)) -->[s].
identifier(t_identifier(t)) -->[t].
identifier(t_identifier(u)) -->[u].
identifier(t_identifier(v)) -->[v].
identifier(t_identifier(w)) -->[w].
identifier(t_identifier(x)) -->[x].
identifier(t_identifier(y)) -->[y].
identifier(t_identifier(z)) -->[z].

numb(t_numb(X)) --> digit(X).
numb(t_numb(X,Y)) -->digit(X), numb(Y).
numb(t_numb(X,Y,Z)) -->digit(X), numb(Y),[.],digit(Z).
numb(t_numb(X,Y)) -->digit(X),[.],digit(Y).

digit(t_digit(0)) -->[0].
digit(t_digit(1)) -->[1].
digit(t_digit(2)) -->[2].
digit(t_digit(3)) -->[3].
digit(t_digit(4)) -->[4].
digit(t_digit(5)) -->[5].
digit(t_digit(6)) -->[6].
digit(t_digit(7)) -->[7].
digit(t_digit(8)) -->[8].
digit(t_digit(9)) -->[9].