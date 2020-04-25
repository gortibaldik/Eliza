
% in dynamic memory there are questions for
% user in case keyword search fails
% we keep memory as FIFO
:- dynamic memory/1.
memory([]).

% get_initial_uninformed_memory_comment(-Comment, -Keyword, -Pattern_index, -Priority)
get_initial_uninformed_memory_comment(Comment, memory, 0,-1) :-
    memory([response(Response)|_]),
    flatten(Response, Comment).


% append new question for user to 
% memory list
append_to_memory_list(Response) :-
    memory(Old),
    append(Old, [Response], New), 
    retract(memory(Old)),
    asserta(memory(New)),
    !.

% remove first question from the list
remove_head_memory_list :-
    memory([X|New]),
    retract(memory([X|New])),
    asserta(memory(New)).

get_random_memory_pattern(Pattern) :- 
    memory_patterns(Patterns), 
    length(Patterns, Length),
    Upper is Length - 1,
    random_between(0, Upper, I),
    nth0(I, Patterns, Pattern).

memory_patterns([
    memory_pattern(matched([_,vas, X]), response([podme, sa, dalej, porozpravat, ',', preco, vas, X, '!'])),
    memory_pattern(matched([_,vas, X]), response([skor, ste, povedali, ',', ze,vas, X])),
    memory_pattern(matched([_,vas, X]), response([preco, hovorite, ',', ze, vas, X, '?'])),
    memory_pattern(matched([_,vas, X]), response([ma, to, nieco, docinenia, s, tym, ',', ze, vas, X, '?']))
]).