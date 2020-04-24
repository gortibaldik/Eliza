% punctuation(+Word)
%   true if Word is punctuation
punctuation('.').
punctuation(',').
punctuation('!').
punctuation('?').
punctuation(';').

lemm_([o,s,p,r,a,v,e|_], "prepac") :- !. 
lemm_([s,o,m], "si") :- !.
lemm_([m,i], "ti") :- !.
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


% remove_stop_words(+[Word|Sentence],-Result )
%   remove stop words from input

