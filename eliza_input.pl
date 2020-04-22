% reading input
% 
% from Natural Language Processing for Prolog Programmers
% Michael A. Covington


% read_atomics(-Atomics)
%   reads a line of text, breaks it into list
%   of atomic terms
read_atomics(Atomics) :-
    read_char(Fchar, Ftype),
    complete_line(Fchar, Ftype, Atomics).

% read_char(-Fchar, -Ftype)
%   reads char from stdin and returns its type
%   and standardized form of char (lowered)
read_char(Char, Type) :-
    get0(C),
    char_type(C, Char, Type).

% char_type(+C, -Char, -Type)
%   operations with C :
%
%   sets up its type 
%       - end (EOL, EOF)
%       - a_n (alpha_numeric)
%       - special (all others)
%
%   case C of "uppercase_letter" 
%       - makes it lowercase

char_type(10, 10, end) :- !. % UNIX EOL
char_type(13, 13, end) :- !. % WIN EOL
char_type(-1, -1, end) :- !. % EOF

char_type(C, 32, blank) :-
    C =< 32, !.             % Controls and blanks

% a_n : alpha_numeric
char_type(C, C, a_n) :-
    48 =< C, C =< 57, !.    % DIGITS

char_type(C, C, a_n) :-
    97 =< C, C =< 122, !.   % LOWERCASE LETTER

char_type(C, NewC, a_n) :-  % UPPERCASE LETTER
    65 =< C, C =< 90, !,
    NewC is C + 32.         % to_lower

% special unrecognized chars
char_type(C, C, special).

% complete_line(+Char, +Type, -Atomics)
%   - based on char and its type returns the line divided into atoms

% EO[FL] - halt
complete_line(_, end, []) :- !. 

% white_spaces - don't consider continue reading
complete_line(_, blank, Atomics) :-
    !, read_atomics(Atomics).

% special characters are treated as single atoms
% since we're dealing only with ASCII, special 
% characters are mostly punctuation 
complete_line(C, special, [A| Atomics]) :-
    !, atom_codes(A, [C]), read_atomics(Atomics). 

% if we encounter alpha_numeric char we leave
%   the task of reading input on predicate 
%   complete_word, which returns CompleteWord
%   as list of chars -> transformation into atom
complete_line(C, a_n, [Word| Atomics]) :-
    complete_word(C, a_n,CompleteWord, NextChar, NextType),
    atom_codes(Word,CompleteWord),
    complete_line(NextChar, NextType, Atomics).

% complete_word(+Char, +Type, -CompleteWord, -NextChar, -NextType)
%   reads word (sequence of alpha_numeric chars) and returns it as [Char]
complete_word(Char, a_n, [Char|Rest], RetChar, RetType) :-
    !, read_char(NextChar, NextType),
    complete_word(NextChar, NextType, Rest, RetChar, RetType).

complete_word(Char, NonAlphaNumericType, [], Char, NonAlphaNumericType).