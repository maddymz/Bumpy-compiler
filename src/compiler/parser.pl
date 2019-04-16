program(t_program(X,Y)) -->block(t_block(X));comment(t_comment(Y)).
program(t_program(X)) -->block(t_block(X)).

comment(t_comment(Y)) --> [@],words(t_words(Y)),[@].

words(t_words(X,Y)) --> identifier(t_identifier(X)), words(t_words(Y)) ; numb(t_numb(X)), words(t_words(Y)).
words(t_words(X)) -->identifier(t_identifier(X)); numb(t_numb(X)).

block(t_block(X,Y)) --> [start], declaration(t_declare(X)), process(t_process(Y)), [end].

declaration(t_declare(X,Y)) --> [var], identifier(t_identifier(X)), [;], declaration(t_declare(Y)) ; [bool], identifier(t_identifier(X)), [;] ,declaration(t_declare(Y)).    
declaration(t_declare(X)) -->[var], identifier(t_identifier(X)); [bool], identifier(t_identifier(X)).

process(t_process(X,Y)) --> assignvalue(t_assign(X)), [;], process(t_process(Y)); control(t_control(X)), [;], process(t_process(Y)); iterate(t_iterate(X)), [;], process(t_process(Y)).
process(t_process(X)) -->assignvalue(t_assign(X)) ;control(t_control(X)) ;iterate(t_iterate(X)).

assignvalue(t_assign(X,Y)) --> identifier(t_identifier(X)), [=] ,expression(t_expr(Y)); identifier(t_identifier(X)), [is], boolexp(t_boolexp(Y)).

control(t_control(X,Y,Z)) --> [incase], condition(t_cond(X)), [do], process(t_process(Y)), [otherwise], process(t_process(Z)), [endcase].

iterate(t_iterate(X,Y)) --> [when], condition(t_cond(X)), [repeat], process(t_process(Y)), [endrepeat].

condition(t_cond(X,Y)) --> boolexp(t_boolexp(X)), [and], boolexp(t_boolexp(Y)); boolexp(t_boolexp(X)), [or], boolexp(t_boolexp(Y)).
condition(t_cond(X)) -->[~], boolexp(t_boolexp(X)); boolexp(t_boolexp(X)).

boolexp(t_boolexp(X,Y)) --> expression(t_expr(X)), [:=:], expression(t_expr(Y)); expression(t_expr(X)), [~=], expression(t_expr(Y)); 
    expression(t_expr(X)), [<=], expression(t_expr(Y)); expression(t_expr(X)), [>=], expression(t_expr(Y)); expression(t_expr(X)), [<], expression(t_expr(Y));
    expression(t_expr(X)), [>], expression(t_expr(Y)); [yes]; [no].

expression(t_expr(X,Y)) --> term(t_term(X)),[+],expression(t_expr(Y)); term(t_term(X)),[-],expression(t_expr(Y)).
expression(t_expr(X)) -->term(t_term(X)).

term(t_term(X,Y)) --> identifier(t_identifier(X)),[*],term(t_term(Y)); numb(t_numb(X)),[*],term(t_term(Y)); identifier(t_identifier(X)),[/],term(t_term(Y)); numb(t_numb(X)),[/],term(t_term(Y));
    identifier(t_identifier(X)),[mod],term(t_term(Y)); numb(t_numb(X)),[mod],term(t_term(Y)).
term(t_term(X)) -->identifier(t_identifier(X)); numb(t_numb(X)).

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

numb(t_numb(X)) --> digit(t_digit(X)).
numb(t_numb(X,Y)) -->digit(t_digit(X)), numb(t_numb(Y)).
numb(t_numb(X,Y,Z)) -->digit(t_digit(X)), numb(t_numb(Y)),[.],digit(t_digit(Z)).
numb(t_numb(X,Y)) -->digit(t_digit(X)),[.],digit(t_digit(Y)).

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