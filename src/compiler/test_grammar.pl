program(t_program(X,Y)) -->comment(X),block(Y).
program(t_program(X)) -->block(X).

comment(t_comment(Y)) --> [@],words(Y),[@].

words(t_words(X,Y)) --> ident(X), words(Y) ; numb(X), words(Y).
words(t_words(X)) -->ident(X); numb(X).

block(t_block(X,Y)) --> [start], declaration(X), process(Y), [stop].

declaration(t_declare(X,Y)) --> [var], ident(X), [;], declaration(Y) ; [bool], ident(X), [;] ,declaration(Y).    
declaration(t_declare(X)) -->[var], ident(X),[;]; [bool], ident(X),[;].

process(t_process(X,Y)) --> assignvalue(X), [;], process(Y); control(X), process(Y); iterate(X), process(Y).
process(t_process(X)) -->assignvalue(X),[;] ;control(X) ;iterate(X).

assignvalue(t_assign(X,Y)) --> ident(X), [=] ,expression(Y); ident(X), [is], boolexp(Y).

control(t_control(X,Y,Z)) --> [incase], condition(X), [do], process(Y), [otherwise], process(Z), [endcase].

iterate(t_iterate(X,Y)) --> [when], condition(X), [repeat], process(Y), [endrepeat].

condition(t_cond(X,Y)) --> boolexp(X), [and], boolexp(Y); boolexp(X), [or], boolexp(Y).
condition(t_cond(X)) -->[~], boolexp(X); boolexp(X).

boolexp(t_boolexp(X,Y)) --> expression(X), [:=:], expression(Y); expression(X), [~=], expression(Y); 
    expression(X), [<=], expression(Y); expression(X), [>=], expression(Y); expression(X), [<], expression(Y);
    expression(X), [>], expression(Y); expression(X), [:=:], boolexp(Y); expression(X), [~=], boolexp(Y). 
boolexp(t_boolexp(yes)) --> [yes].
boolexp(t_boolexp(no)) --> [no].

expression(t_expr(X,Y)) --> term(X),[+],expression(Y); term(X),[-],expression(Y).
expression(t_expr(X)) --> term(X).

term(t_term(X)) -->ident(X); numb(X).
term(t_term(X,Y)) --> ident(X),[*],term(Y); numb(X),[*],term(Y); ident(X),[/],term(Y); numb(X),[/],term(Y);
    ident(X),[mod],term(Y); numb(X),[mod],term(Y).

ident(t_identifier(X)) --> identifier(X).

identifier(Y) -->[a],identifier(X),list_concat([a,X],Y).
identifier(Y) -->[b],identifier(X),list_concat([b,X],Y).
identifier(Y) -->[c],identifier(X),list_concat([c,X],Y).
identifier(Y) -->[d],identifier(X),list_concat([d,X],Y).
identifier(Y) -->[e],identifier(X),list_concat([e,X],Y).
identifier(Y) -->[f],identifier(X),list_concat([f,X],Y).
identifier(Y) -->[g],identifier(X),list_concat([g,X],Y).
identifier(Y) -->[h],identifier(X),list_concat([h,X],Y).
identifier(Y) -->[i],identifier(X),list_concat([i,X],Y).
identifier(Y) -->[j],identifier(X),list_concat([j,X],Y).
identifier(Y) -->[k],identifier(X),list_concat([k,X],Y).
identifier(Y) -->[l],identifier(X),list_concat([l,X],Y).
identifier(Y) -->[m],identifier(X),list_concat([m,X],Y).
identifier(Y) -->[n],identifier(X),list_concat([n,X],Y).
identifier(Y) -->[o],identifier(X),list_concat([o,X],Y).
identifier(Y) -->[p],identifier(X),list_concat([p,X],Y).
identifier(Y) -->[q],identifier(X),list_concat([q,X],Y).
identifier(Y) -->[r],identifier(X),list_concat([r,X],Y).
identifier(Y) -->[s],identifier(X),list_concat([s,X],Y).
identifier(Y) -->[t],identifier(X),list_concat([t,X],Y).
identifier(Y) -->[u],identifier(X),list_concat([u,X],Y).
identifier(Y) -->[v],identifier(X),list_concat([v,X],Y).
identifier(Y) -->[w],identifier(X),list_concat([w,X],Y).
identifier(Y) -->[x],identifier(X),list_concat([x,X],Y).
identifier(Y) -->[y],identifier(X),list_concat([y,X],Y).
identifier(Y) -->[z],identifier(X),list_concat([z,X],Y).

identifier(a) -->[a],list_concat([a],_).
identifier(b) -->[b],list_concat([b],_).
identifier(c) -->[c],list_concat([c],_).
identifier(d) -->[d],list_concat([d],_).
identifier(e) -->[e],list_concat([e],_).
identifier(f) -->[f],list_concat([f],_).
identifier(g) -->[g],list_concat([g],_).
identifier(h) -->[h],list_concat([h],_).
identifier(i) -->[i],list_concat([i],_).
identifier(j) -->[j],list_concat([j],_).
identifier(k) -->[k],list_concat([k],_).
identifier(l) -->[l],list_concat([l],_).
identifier(m) -->[m],list_concat([m],_).
identifier(n) -->[n],list_concat([n],_).
identifier(o) -->[o],list_concat([o],_).
identifier(p) -->[p],list_concat([p],_).
identifier(q) -->[q],list_concat([q],_).
identifier(r) -->[r],list_concat([r],_).
identifier(s) -->[s],list_concat([s],_).
identifier(t) -->[t],list_concat([t],_).
identifier(u) -->[u],list_concat([u],_).
identifier(v) -->[v],list_concat([v],_).
identifier(w) -->[w],list_concat([w],_).
identifier(x) -->[x],list_concat([x],_).
identifier(y) -->[y],list_concat([y],_).
identifier(z) -->[z],list_concat([z],_).

list_concat(A,Y,_,_):- atomic_list_concat(A,"",Y).

numb(t_numb(X)) --> digit(X).
numb(t_numb(X,Y)) --> digit(X), numb(Y).
numb(t_numb(X,Y,Z)) --> digit(X), numb(Y),[.],digit(Z).
numb(t_numb(X,Y)) --> digit(X),[.],digit(Y).

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