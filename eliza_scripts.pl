% list of all scripts used by Eliza to answer user prompts
% and utility functions to use them


just_write_something() :-
    write('SOMETHING ! \n').

% get_initial_uninformed_comment(-Initial_comment, -Keyword, -Pattern_index)
get_initial_uninformed_comment(Initial_comment, none, Pattern_index) :-
    Pattern_index = 0,

    % we get initial uninformed comment from Eliza, from none_script
    get_action(none, Pattern_index, Action_index),
    none_script(None_script),
    get_script_pattern_by_index(None_script, Pattern_index, Pattern),
    get_pattern_actions(Pattern, Actions),

    % built-in
    % get nth element of Actions list and store it
    % into Action
    nth0(Action_index, Actions, response(Comment)),

    % comments are in format ec((Elizas comment, keyword, PatternIndex), priority, 
    Initial_comment = ec((Comment, none, Pattern_index), -1, 0).


% get_informed_comment(+Script, -Comment, -Keyword, -Pattern_index)
get_informed_comment(Script, Comment, Keyword, Pattern_index) :-
    % used as getter
    script_contains_keyword(Script, Keyword),

    % only temporary, not resolved yet
    Pattern_index = 0,
    get_action(Keyword, Pattern_index, Action_index),
    get_script_pattern_by_index(Script, Pattern_index, Pattern),
    get_pattern_actions(Pattern, Actions),

    % built-in
    % get nth element of Actions list and store it
    % into Action
    nth0(Action_index, Actions, response(Comment)).

assert_next_action(Keyword, Pattern_index) :-
    (
        Keyword = none -> none_script(Script);
        script_contains_keyword(Script, Keyword)
    ),
    get_script_patterns(Script, Patterns),

    % built-in
    % get nth element of Patterns list and store it
    % into Pattern
    nth0(Pattern_index, Patterns, Pattern),
    get_pattern_actions(Pattern, Actions),
    get_action(Keyword, Pattern_index, Action_index),
    length(Actions, Length),

    NAI is ( Action_index + 1 ) mod Length,
    retract(memory_current_action(Keyword, Pattern_index, Action_index)),
    asserta(memory_current_action(Keyword, Pattern_index, NAI)).

    

% script_contains_keyword(+Script, -Keyword)
script_contains_keyword(Script, Keyword) :-
    scripts(Script),
    Script = script(keyword(Keyword, _),_).

% get_action(+Keyword, +Pattern_index, -Action_index)
%   gets action stored in memory_current_action
%   or resets it if new action is encountered
get_action(Keyword, Pattern_index, Action_index) :-
    memory_current_action(Keyword, Pattern_index, A) -> Action_index = A;
    Action_index = 0, asserta(memory_current_action(Keyword, Pattern_index, Action_index)).
    
get_scripts_matching_keywords(User_input, Sorted) :-
    findall(Script, (scripts(Script), script_contains_keyword(Script, Keyword), member(Keyword, User_input)), MatchedScripts),
    merge_sort(MatchedScripts, Sorted).


get_script_patterns(Script, Patterns) :-
    Script = script(keyword(_,_), Patterns).

% get_script_pattern_by_index(+Script, +Pattern_index, -Pattern)
get_script_pattern_by_index(Script, Pattern_index, Pattern) :-
    get_script_patterns(Script,Patterns),
    
    % built-in
    % get nth element of Patterns list and store it
    % into Pattern
    nth0(Pattern_index, Patterns, Pattern).

get_pattern_actions(Pattern, Actions) :-
    Pattern = pattern(_, actions(Actions)).

get_script_priority(Script, Priority) :-
    Script = script(keyword(_,Priority),_).

get_ec_comment(ec(Response, _,_), Response).

get_ec_keyword_priority(ec(_,Priority,_), Priority).

get_ec_pattern_index(ec(_,_,Score), Score).

% we use memory_current_action as a predicate 
% with action to be performed when
% nothing betters appears to be done

% memory_current_action(Keyword, Pattern_index, Action_index)
:- dynamic memory_current_action/3.
memory_current_action(_,_,0).




% first script, called at the start of running of Eliza
start_script([dobry, den, porozpravajte, mi, o, vasom, probleme, '!']).

% quit script, called at the end of running
quit_script([dakujem, za, podnetny, rozhovor, '.', dufam, ',', ze, sa, este, niekedy, stretneme, '.']).

% none_script, called if Eliza hadn't understood what's going on
none_script(
    script(
        keyword(none, 0),
        [
            pattern(
                matched([_]),
                actions([
                    response([nemyslim, si, ',', ze, vam, uplne, rozumiem, '.']),
                    response([pokracujte, '.']),
                    response([citite, sa, dobre, ',', ked, rozpravate, o, takychto, veciach, '?'])
                ])
            )
        ]
    )
).

% sorry_script, called after detection of word sorry - "prepac"
scripts(
    script(
        keyword(prepac, 0),
        [
            pattern(
                matched([_]),
                actions([
                    response([prosim, ',', neospravedlnujte, sa, '.']),
                    response([nie, je, potrebne, ospravedlnovat, sa, '.']),
                    response([ako, sa, citite, ',', ked, sa, ospravedlnujete, '?']),
                    response([povedal, som, vam, ',', aby, ste, sa, neospravedlnovali, '.'])
                ])
            )
        ]
    )
).

halve(L,A,B) :- halve_(L,L,A,B).

halve_([], R, [], R). % 1.
halve_([_], R, [], R). % 2.
halve_([_,_|T], [X|L], [X|L1], R) :- halve_(T, L, L1, R).

merge(XS, [], XS) :- !.
merge([], YS, YS) :- !.
merge([XScript|XS], [YScript|YS], R) :-
    get_script_priority(XScript, X),
    get_script_priority(YScript, Y),
    ( X @=< Y -> merge(XS, [Y|YS], S), R = [X|S]
    ; merge([X|XS], YS, S), R = [Y|S]
    ).

merge_sort([], []) :- !.
merge_sort([X], [X]) :- !.
merge_sort(L, S) :-
  L = [_,_|_],
  halve(L, LL, LR),
  merge_sort(LL, LS),
  merge_sort(LR, RS),
  merge(LS, RS, S).