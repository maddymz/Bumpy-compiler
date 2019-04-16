program --> comment, block.
comment --> [@],words,[@] ; [].
words --> identifier, words ; number, words ; identifier; number.
block --> [start], declaration, process, [end].
declaration --> [var], identifier, [;], declaration ; [bool], identifier, [;] declaration;
    [var], identifier; [bool], identifier.
process --> assignvalue, [;], process; control, [;], process; iterate, [;], process;
    assignvalue ;control ;iterate.
assignvalue --> identifier, [=] ,expression; identifier, [is], boolexp.
control --> [incase], condition, [do], process, [otherwise], process, [endcase].
iterate --> [when], condition, [repeat], process, [endrepeat].
condition --> boolexp, [and], boolexp; boolexp, [or], boolexp; [~], boolexp; boolexp.
boolexp --> expression, [:=:], expression; expression, [~=], expression; 
    expression, [<=], expression; expression, [>=], expression; expression, [<], expression;
    expression, [>], expression; [yes]; [no].
expression --> term,[+],expression; term,[-],expression; term.
term --> identifier,[*],term; number,[*],term; identifier,[/],term; number,[/],term;
    identifier,[mod],term; number,[mod],term; identifier; number.
identifier --> [a..z], identifier; [a..z].
