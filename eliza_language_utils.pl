% punctuation(+Word)
%   true if Word is punctuation
punctuation('.').
punctuation(',').
punctuation('!').
punctuation('?').
punctuation(';').
punctuation(':').

transform_([s,p,o,m,i,n,a|X], [p,a,m,a,t,a|X]) :-!.
transform_(X,X).


transform([s,o,m], "ste") :- !.
transform([s,i], "si") :-!.
transform([s,t,e], "som") :- !.
transform(X, Y) :-
    conjugation(X, sg2, now, Z),
    append(Z, [m],Y),!.
transform(X,Y) :-
    conjugation(X, pl2, now, Z),
    append(Z, [m],Y),!.
transform(X,Y) :-
    conjugation(X, sg1, now, Z),
    append(Z, [t,e],Y), !.
    
transform([m,o,j|X], [v,a,s|X]) :- !.
transform([n,a,s|X], [v,a,s|X]) :- !.
transform([m,i], "vam") :- !.
transform([t,v,o,j|X], [m,o,j|X]) :-!.
transform([v,a,s|X], [m,o,j|X]) :-!.
transform(X,X).



stem_lemm(Atom, Stemmed) :-
    atom_chars(Atom, Chars),
    transform_(Chars, Chars_1),
    transform(Chars_1, Chars_lemm),
    atom_chars(Stemmed, Chars_lemm).

traverse_input_stem_lemm([Word], []) :-
    punctuation(Word), !.

traverse_input_stem_lemm([Word|Rest], [Result| ResultRest]) :-
    stem_lemm(Word, Result),
    traverse_input_stem_lemm(Rest, ResultRest).

traverse_input_stem_lemm([],[]).


% --- keyword matching ---
% used only in keyword matching phase
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
conditional_lemm([s, n, _], [s,e,n]) :-!.
conditional_lemm([s,n,o,m], [s,e,n]) :-!.
conditional_lemm([s,n,o|[_|_]], [s,e,n]) :- !.
conditional_lemm([s,n,a,m,i], [s,e,n]) :-!.
conditional_lemm([s,n,i,v,a|_], [s,n,i,v,a,t]) :-!.
conditional_lemm([v,s,e,t,c,i], [k,a,z,d,y]) :- !.
conditional_lemm([n,i,k,t,o], [k,a,z,d,y]) :- !.
conditional_lemm([p,a,m,a,t,a|_], [p,a,m,a,t,a,t]) :- !.
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
%   used to find out grammatical gender,
%   conjugations and declensions 
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
% --- conjugations ---
conjugation(X, neuter, neuter,Y) :-
    append(Y, [t], X), !.

conjugation(X, sg, past,Y) :-
    append(Y, [l], X), !.

conjugation(X, sg, past,Y) :-
    append(Y, [a,l,C], X), !,
    (
        C = a;
        C = o
    ).

conjugation(X, pl, past,Y) :-
    append(Y, [l, i], X), !.

conjugation(X, sg1,now, Y) :-
    append(Z, [A, m], X), !, 
    (
        A = i, !;
        A = e, !;
        A = a, !;
        A = o
    ), append(Z, [A],Y).

conjugation([s,i], sg2, now,[s]) :-!.
conjugation(X, sg2, now,Y) :-
    append(Z, [A, s], X),!, 
    (
        A = e, !;
        A = a, !;
        A = i
    ), append(Z,[A],Y).

conjugation(X, pl1, now,Y) :-
    append(Y, [m,e], X), !.

conjugation(X, pl2, now,Y) :-
    append(Y, [t,e], X), !.

conjugation(X, sg3, now,Y) :-
    append(Y, [e], X), !.

conjugation(X, sg3, now,Y) :-
    append(Y, [a], X), !.

conjugation(X, sg3, now,Y) :-
    append(Y, [i], X), !.

conjugation(X, pl3, now, Y) :-
    append(Y, [u], X), !.

conjugation(X, pl3, now, Y) :-
    append(Y, [i,a], X), !.

conj(X, Y, Z) :-
    atom_chars(X, CX),
    conjugation(CX, Y, Z,_).
% ---
% --- possessive matching ---
possessive_([m,o,j|_], i).
possessive_([t,v,o,j|_], y).
possessive_([n,a,s|_], we).
possessive_([v,a,s|_], y).
possessive_([j,e,h,o], he).
possessive_([j,e,j], she).
possessive_([i,c,h], they).

possessive(X, Person) :-
    atom_chars(X, Chars), 
    possessive_(Chars, Person).

% --- reflexive matching ---
reflexive(sa).
reflexive(si).


% --- family_script ---

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

% --- dream script ---
dream(Atom, Number) :-
    atom_chars(Atom, Chars),
    dream_(Chars, Number).
dream_([s,e,n], sg) :-!.
dream_([s,n,y], pl) :-!.
dream_([s,n,_], sg) :-!.
dream_([s,n,o|[_|_]], pl) :-!.
dream_([s,n,o,m], sg) :-!. 
dream_([s,n,a,m,i], pl) :-!.

dream(Atom, Number, Time) :-
    atom_chars(Atom, Chars), 
    Chars = [s,n,i,v,a|_],
    conjugation(Chars, Number, Time,_).

remember(Atom, Number, Time) :-
    atom_chars(Atom,Chars),
    Chars = [p,a,m,a,t,a|_],
    conjugation(Chars, Number, Time, _).