program(t_program(X,Y)) -->comment(X),block(Y).
program(t_program(X)) -->block(X).

comment(t_comment(Y)) --> [@],words(Y),[@].

words(t_words(X,Y)) --> identifier(X), words(Y) ; numb(X), words(Y).
words(t_words(X)) -->identifier(X); numb(X).

block(t_block(X,Y)) --> [start], declaration(X), process(Y), [stop].

datatype(t_datatype(var)) --> [var].
datatype(t_datatype(bool)) --> [bool].

declaration(t_declare(X,Y,Z)) --> datatype(X), identifier(X), [;], declaration(Z).    
declaration(t_declare(X,Y)) -->datatype(X),identifier(Y),[;].

process(t_process(X,Y)) --> assignvalue(X), [;], process(Y); control(X), process(Y); iterate(X), process(Y).
process(t_process(X)) -->assignvalue(X),[;] ;control(X) ;iterate(X).

assignvalue(t_assign(X,Y)) --> identifier(X), [=] ,expression(Y); identifier(X), [is], boolexp(Y).

control(t_control(X,Y,Z)) --> [incase], condition(X), [do], process(Y), [otherwise], process(Z), [endcase].

iterate(t_iterate(X,Y)) --> [when], condition(X), [repeat], process(Y), [endrepeat].

condition(t_cond(X,Y)) --> boolexp(X), [and], boolexp(Y); boolexp(X), [or], boolexp(Y).
condition(t_cond(X)) -->[~], boolexp(X); boolexp(X).

boolexp(t_boolexp(X,Y)) --> expression(X), [:=:], expression(Y); expression(X), [~=], expression(Y); 
    expression(X), [<=], expression(Y); expression(X), [>=], expression(Y); expression(X), [<], expression(Y);
    expression(X), [>], expression(Y); expression(X), [:=:], boolexp(Y); expression(X), [~=], boolexp(Y). 
boolexp(t_boolexp(yes)) --> [yes].
boolexp(t_boolexp(no)) --> [no].

expression(t_add(X,Y)) --> term(X),[+],expression(Y).
expression(t_sub(X,Y)) --> term(X),[-],expression(Y).
expression(t_expr(X)) --> term(X).

term(t_mul(X,Y)) --> factor(X),[*],term(Y).
term(t_div(X,Y)) -->factor(X),[/],term(Y).
term(t_mod(X,Y)) -->factor(X),[mod],term(Y).
term(t_term(X)) -->factor(X).

factor(t_identifier(X)) -->identifier(X).
factor(t_numb(X)) -->numb(X).
factor(t_numbneg(X)) -->numbneg(X).

identifier(t_identifier(X)) -->[X], 
    {string_chars(X,[Fc|Rc])},{(is_alpha(Fc);X='_')},
    {forall(member(R,Rc),(is_alnum(R)); R = '_')}.

numb(t_numb(X)) --> [X],{number(X)}.

numbneg(t_numbneg(X)) --> [-],numb(X).
