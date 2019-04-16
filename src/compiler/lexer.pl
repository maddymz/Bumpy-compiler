program --> comment, block.
comment --> [@],words,[@] ; [].
words --> identifier, words ; numb, words ; identifier; numb.
block --> [start], declaration, process, [end].
declaration --> [var], identifier, [;], declaration ; [bool], identifier, [;] ,declaration;
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
term --> identifier,[*],term; numb,[*],term; identifier,[/],term; numb,[/],term;
    identifier,[mod],term; numb,[mod],term; identifier; numb.
%identifier --> [a],identifier;[b],identifier;[c],identifier;[d],identifier;[e],identifier;[f],identifier;[g],identifier;[h],identifier;[i],identifier;[j],identifier;[k],identifier;[l],identifier;[m],identifier;[n],identifier;[o],identifier;[p],identifier;[q],identifier;[r],identifier;[s],identifier;[t],identifier;[u],identifier;[v],identifier;[w],identifier;[x],identifier;[y],identifier;[z],identifier.
identifier -->[a];[b];[c];[d];[e];[f];[g];[h];[i];[j];[k];[l];[m];[n];[o];[p];[q];[r];[s];[t];[u];[v];[w];[x];[y];[z].
numb --> digit;digit, numb;digit, numb,[.],digit;
    digit,[.],digit.
digit --> [0];[1];[2];[3];[4];[5];[6];[7];[8];[9].