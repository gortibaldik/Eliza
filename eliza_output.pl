% writing output
% comment - Elizas comment of the user's input
%           written to the stdout

% using eliza_language_utils module
% there punctuation is defined
:- ensure_loaded(eliza_language_utils).


comment(Sentence) :- comment_(Sentence, first).


% comment_(+ListOfWordsToWrite, +Mark)
%   after having read all the words, print new_line
comment_([], _) :- 
    nl,!.

%   1. Mark = first -> start the sentence with upcase_letter
%   2. write all the other words
comment_([Word| Sentence], first) :-
    upcase_first_letter(Word, Upper),!,
    write(Upper),
    comment_(Sentence, next).

comment_([Word|Sentence], sentence) :-
    upcase_first_letter(Word, Upper),!,
    format(' ~w',[Upper]),
    comment_(Sentence, next).

%   1. Mark = next ->
%       word is punctuation -> don't write space before it
%       otherwise write space before it
%   2. write all the other words
comment_([Word| Sentence], next) :-
    (
        punctuation(Word),!,
        ( 
            Word = '.',!,NextTag = sentence;
            NextTag = next
        );            
        write(' '), NextTag = next
    ),
    write(Word),
    comment_(Sentence, NextTag).

% upcase_first_letter(+Word, -Returned)
%   each word is stored as atom -> lowercase or 
%   punctuation/digit in quotes
%   -> upcase first letter in atom
upcase_first_letter(Word, Returned) :-
    atom_chars(Word, [FCode|Codes]),
    upcase_atom(FCode, Upper),
    atom_chars(Returned, [Upper| Codes]).