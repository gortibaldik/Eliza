% punctuation(+Word)
%   true if Word is punctuation
punctuation('.').
punctuation(',').
punctuation('!').
punctuation('?').
punctuation(';').
punctuation(':').

lemm_([o,s,p,r,a,v,e|_], "prepac") :- !. 
lemm_([s,o,m], "ste") :- !.
lemm_([m,o,j|X], [v,a,s|X]) :- !.
lemm_([m,i], "vam") :- !.
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

% is_declination(+X, +Y)
%   returns true if X is declination of Y
is_declination(X, Y) :-
    atom_chars(X, CharsX), 
    atom_chars(Y, CharsY),
    append(CharsY, _, CharsX).

member_declined(Word, [Word|_]) :-!.

member_declined(Word, [Input_Head|_]) :-
    is_declination(Input_Head, Word), !.

member_declined(Word, [_| Input_rest]) :-
    member_declined(Word, Input_rest).


% remove_stop_words(+[Word|Sentence],-Result )
%   remove stop words from input

family(brat).
family(sestr).
family(otec).
family(otc).
family(mam).
family(sesternic).
family(bratranc).
family(stryk).
family(tet).

% force at least 1 char after base of declination
family_declined_([b,r,a,t|[_|_]]).
family_declined_([s,e,s,t,r|[_|_]]).
family_declined_([o,t,c|[_|_]]).
family_declined_([m,a,m|[_|_]]).

family_declined(X) :-
    atom_chars(X, Chars),
    family_declined_(Chars).