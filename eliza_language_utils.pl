% punctuation(+Word)
%   true if Word is punctuation
punctuation('.').
punctuation(',').
punctuation('!').
punctuation('?').
punctuation(';').
punctuation(':').

lemm_([k,a,z,d,y], [v,s,e,t,c,i]) :-!.

lemm_([p,a,m,a,t,a,s], [p,a,m,a,t,a,m]) :- !.
lemm_([p,a,m,a,t,a,t,e], [p,a,m,a,t,a,m]) :- !.
lemm_([p,a,m,a,t,a,m], [p,a,m,a,t,a,t,e]) :- !.


lemm_([s,o,m], "ste") :- !.
lemm_([s,t,e], "som") :- !.
lemm_([m,o,j|X], [v,a,s|X]) :- !.
lemm_([m,i], "vam") :- !.
lemm_([t,v,o,j|X], [m,o,j|X]) :-!.
lemm_([v,a,s|X], [m,o,j|X]) :-!.
lemm_(X,X).

stem_([p,r,e,p,a,c|_], "prepac") :-!.
stem_(X,X).



stem_lemm(Atom, Stemmed) :-
    atom_chars(Atom, Chars),
    stem_(Chars, Chars_stem),
    lemm_(Chars_stem, Chars_lemm),
    atom_chars(Stemmed, Chars_lemm).

traverse_input_stem_lemm([Word|Rest], [Result| ResultRest]) :-
    stem_lemm(Word, Result),
    traverse_input_stem_lemm(Rest, ResultRest).

traverse_input_stem_lemm([],[]).


% --- keyword matching ---
no_declination(ak).

conditional_lemm([p,r,e,p,a,c|_], [p,r,e,p,a,c]) :- !.
conditional_lemm([o,s,p,r,a,v,e|_], [p,r,e,p,a,c]) :- !.
conditional_lemm([b,r,a,t|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([s,e,s,t,r|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([o,t,e,c|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([o,t,c|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([m,a,m|_], [f,a,m,i,l,y]) :-!.
conditional_lemm([s,e,s,t,e,r,n,i,c|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([b,r,a,t,r,a,n|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([s,t,r,y,k|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([t,e,t|_], [f,a,m,i,l,y]) :- !.
conditional_lemm([d,e,d|_], [f,a,m,i,l,y]) :-!.
conditional_lemm([t,a,t,k|_], [f,a,m,i,l,y]) :-!.
conditional_lemm([b,a,b,k|_], [f,a,m,i,l,y]) :- !.
conditional_lemm(X,X).
% is_declination(+X, +Y)
%   returns true if X is declination of Y
%   we need not decline words like "ak" -> "ako"
%   because of their different meanings
is_declination(X, Y) :-
    \+no_declination(Y),
    atom_chars(X, CharsX), 
    atom_chars(Y, CharsY),
    (append(CharsY, _, CharsX) -> true;
    conditional_lemm(CharsX, CharsY)).

member_declined(Word, [Word|_]) :-!.

member_declined(Word, [Input_Head|_]) :-
    is_declination(Input_Head, Word), !.

member_declined(Word, [_| Input_rest]) :-
    member_declined(Word, Input_rest).
% ----
% --- noun matching ---

hard(h).
hard(k).
hard(g).
hard(d).
hard(t).
hard(n).
hard(l).

hard(b).
hard(p).
hard(m).
hard(r).
hard(s).
hard(v).
hard(z).

soft(c).
soft(j).

gram_case_feminine_sg(X, n) :-
    append(_, [a], X), !.

gram_case_feminine_sg(X,g) :-
    append(_, [Conson, Y], X), 
    (
        hard(Conson) -> Y = y;
        soft(Conson) -> Y = e;
        fail
    ), !.

gram_case_feminine_sg(X,d) :-
    append(_, [Conson, Y], X), 
    (
        hard(Conson) -> Y = e;
        soft(Conson) -> Y = i;
        fail
    ).

gram_case_feminine_sg(X,a) :-
    append(_, [u], X), !.

gram_case_feminine_sg(X,l) :-
    append(_, [Conson, Y], X), 
    (
        hard(Conson) -> Y = e;
        soft(Conson) -> Y = i;
        fail
    ), !.

gram_case_feminine_sg(X,i) :-
    append(_, [o,u], X), !.


gram_case_masculine_sg(X, ga) :-
    append(_,[a], X),!.

gram_case_masculine_sg(X, dl) :-
    append(_,[o,v,i], X),!.

gram_case_masculine_sg(X, i) :-
    append(_,[o,m], X),!.

gram_case_masculine_sg(X,n) :-
    append(_,[Conson], X),
    (soft(Conson);hard(Conson)),!.

gram_case_masculine_sg([d,e,d,o],n) :- !.
gram_case_masculine_sg([t,a,t,k,o],n) :- !.

% ---
% --- possessive matching ---
possessive_([m,o,j|_]).
possessive_([t,v,o,j|_]).
possessive_([n,a,s|_]).
possessive_([v,a,s|_]).
possessive_([j,e,h,o]).
possessive_([j,e,j]).
possessive_([i,c,h]).

possessive(X) :-
    atom_chars(X, Chars), 
    possessive_(Chars).

% force at least 1 char after base of declination
family_feminine_([m,a,m|[_|_]]) :-!.
family_feminine_([m,a,t,k|[_|_]]) :-!.
family_feminine_([s,e,s,t,r|[_|_]]) :-!.
family_feminine_([t,e,t|[_|_]]) :-!.
family_feminine_([s,e,s,t,e,r,n,i,c|[_|_]]) :-!.
family_feminine_([b,a,b,k|[_|_]]) :-!.

family_masculine_([b,r,a,t|[_|_]]) :-!.
family_masculine_([o,t,c|[_|_]]) :-!.
family_masculine_([o,t,e,c]) :-!.
family_masculine_([t,a,t,k|[_|_]]) :-!.
family_masculine_([s,t,r,y,k|[_|_]]) :-!.
family_masculine_([b,r,a,t,r,a,n|[_|_]]) :-!.
family_masculine_([d,e,d|[_|_]]) :-!.

family_feminine(X, Gram_case) :-
    atom_chars(X, Chars),
    family_feminine_(Chars),
    gram_case_feminine_sg(Chars, Gram_case).

family_masculine(X, Gram_case) :-
    atom_chars(X, Chars),
    family_masculine_(Chars),
    gram_case_masculine_sg(Chars, Gram_case).

everybody(vsetci).
everybody(nikto).