tokens(Z) --> "start", tokens(Y), {Z = [start | Y]}.
tokens(Z) --> "when", tokens(Y), {Z = [when | Y]}.
tokens(Z) --> "repeat", tokens(Y), {Z = [repeat | Y]}.
tokens(Z) --> "endrepeat", tokens(Y), {Z = [endrepeat | Y]}.
tokens(Z) --> "incase", tokens(Y), {Z = [incase | Y]}.
tokens(Z) --> "do", tokens(Y), {Z = [do | Y]}.
tokens(Z) --> "otherwise", tokens(Y), {Z = [otherwise | Y]}.
tokens(Z) --> "endcase", tokens(Y), {Z = [endcase | Y]}.
tokens(Z) --> "end", tokens(Y), {Z = [end | Y]}.
tokens(Z) --> "other", tokens(Y), {Z = [other | Y]}.

% comment symbol
tokens(Z) --> "@", tokens(Y), {Z = [@ | Y]}.

% Comparison operators.
tokens(Z) --> ":=:", tokens(Y), {Z = [:=: | Y]}.
tokens(Z) --> "~=", tokens(Y), {Z = [~= | Y]}.
tokens(Z) --> ">", tokens(Y), {Z = [> | Y]}.
tokens(Z) --> "<", tokens(Y), {Z = [< | Y]}.
tokens(Z) --> "<=", tokens(Y), {Z = [<= | Y]}.
tokens(Z) --> ">=", tokens(Y), {Z = [=> | Y]}.

% Arithmetic operators.
tokens(Z) --> "+", tokens(Y), {Z = [+ | Y]}.
tokens(Z) --> "-", tokens(Y), {Z = [- | Y]}.
tokens(Z) --> "*", tokens(Y), {Z = [* | Y]}.
tokens(Z) --> "/", tokens(Y), {Z = [/ | Y]}.

% Assignment operator.
tokens(Z) --> "=", tokens(Y), {Z = [= | Y]}.  

% primitive types.
tokens(Z) --> "var", tokens(Y), {Z = [var | Y]}.
tokens(Z) --> "bool", tokens(Y), {Z = [bool | Y]}.

% Boolean constants and operators.
tokens(Z) --> "true", tokens(Y), {Z = [true | Y]}.  
tokens(Z) --> "false", tokens(Y), {Z = [false | Y]}.  
tokens(Z) --> "and", tokens(Y), {Z = [and | Y]}.  
tokens(Z) --> "or", tokens(Y), {Z = [or | Y]}.  

% Strip spaces, tabs and newlines.
tokens(Z) --> " ", tokens(Y), {Z = Y}.
tokens(Z) --> "	", tokens(Y), {Z = Y}.

% Anything not mentioned above gets its own token,
% including single-character identifiers.
tokens(Z) --> [C], tokens(Y), {name(X, [C]), Z = [X | Y]}.
tokens(Z) --> [], {Z = []}.
