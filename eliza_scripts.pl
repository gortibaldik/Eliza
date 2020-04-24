% list of all scripts used by Eliza to answer user prompts
% and utility functions to use them


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


% get_informed_comment(+User_input, +Script, -Comment, -Keyword, -Pattern_index)
get_informed_comment(User_input, Script, Comment, Keyword, Pattern_index) :-
    % not only find pattern_index, but also 
    % unifies variables in response with user_input
    find_matching_pattern(User_input, Script, Pattern_index),

    % used as getter of Keyword
    script_contains_keyword(Script, Keyword),

    get_action(Keyword, Pattern_index, Action_index),
    get_script_pattern_by_index(Script, Pattern_index, Pattern),
    get_pattern_actions(Pattern, Actions),

    % built-in
    % get nth element of Actions list and store it
    % into Action
    nth0(Action_index, Actions, response(C)),
    flatten(C,Comment).

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

% --- WORKING WITH SCRIPTS : ---  

% script_contains_keyword(?Script, ?Keyword)
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

% pattern_is_matching(+Pattern, +Matched)
get_pattern_match(Pattern, Matched) :-
    Pattern = pattern(Matched, _).

get_pattern_actions(Pattern, Actions) :-
    Pattern = pattern(_, actions(Actions)).

get_script_priority(Script, Priority) :-
    Script = script(keyword(_,Priority),_).

get_ec_comment(ec(Response, _,_), Response).

get_ec_keyword_priority(ec(_,Priority,_), Priority).

get_ec_pattern_index(ec(_,_,Score), Score).

find_matching_pattern(User_input, Script, Pattern_index) :-
    get_script_patterns(Script, Patterns),
    find_matching_pattern_(User_input, Patterns, 0, Pattern_index).

find_matching_pattern_(User_input, [Pattern|Rest], I, Pattern_index) :-
    get_pattern_match(Pattern, matched(Matched)),
    (
        match(User_input, Matched) -> Pattern_index = I;
        NI is I + 1, find_matching_pattern_(User_input, Rest, NI, Pattern_index)
    ).

% match(+User_input, +Pattern)
match(User_input, [Pattern_head|Pattern_tail]) :-
    var(Pattern_head) -> match_variable(User_input, Pattern_tail, Pattern_head, [], Rest_of_input, Rest_of_pattern), match(Rest_of_input, Rest_of_pattern);
    User_input = [Pattern_head|Ui_tail], match(Ui_tail, Pattern_tail).

match([], []).

% match_variable(+User_input, +Pattern, +Pattern_head, +Acc, -Rest_of_input, -Rest_of_pattern)
match_variable(User_input, [Pat_head|Pat_tail], Var, Acc, User_input, [Pat_head|Pat_tail]) :-
    var(Pat_head),!, reverse(Acc, Var).

match_variable([Inp_head|Inp_tail], [Inp_head|Pat_tail], Var, Acc, Rest_of_input, Rest_of_pattern) :-
    !,consume_not_var(Inp_tail, Pat_tail, Rest_of_input, Rest_of_pattern),
    reverse(Acc, Var).

match_variable([Inp_head|Inp_tail], Pattern, Var, Acc, Rest_of_input, Rest_of_pattern) :-
    !,match_variable(Inp_tail, Pattern, Var, [Inp_head| Acc], Rest_of_input, Rest_of_pattern).

match_variable(User_input, [], Var, Acc, [], []) :-
    reverse(Acc, Reversed), 
    append(Reversed, User_input, Var).

consume_not_var([], [], [], []).

consume_not_var(User_input, [Pattern_head| Pattern_tail], User_input, [Pattern_head| Pattern_tail]) :-
    var(Pattern_head), !.

consume_not_var([Inp_head|Inp_tail], [Inp_head|Pat_tail], Rest_of_input, Rest_of_pattern) :-
    consume_not_var(Inp_tail, Pat_tail, Rest_of_input, Rest_of_pattern).


% we use memory_current_action as a predicate 
% with action to be performed when
% nothing betters appears to be done

% memory_current_action(Keyword, Pattern_index, Action_index)
:- dynamic memory_current_action/3.
memory_current_action(_,_,0).

% ---

% --- SCRIPTS ---
% to be able to call script and get important properties of it 
% we keep the following structure :
% scripts(
%   script(
%       keyword(keyword, priority),
%       [pattern_array]
%       )
%   )
% in pattern array there are patterns to be matched
% and Eliza's responses to matched patterns

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

% if_script, called after detection of word if - "ak"
scripts(
    script(
        keyword(ak, 3),
        [
            pattern(
                matched([_, ak, by,si, Y]),
                actions([
                    response([co, by, sa, stalo, ak, by, som, Y]),
                    response([dufas, ze, by, som, Y, '?']),
                    response([naozaj, si, myslis, ',', ze, by, som, Y, '?'])
                ])
            ),
            pattern(
                matched([_, ak, by, som, Y]),
                actions([
                    response([co, by, sa, stalo, ak, by, si, Y, '?']),
                    response([dufas, ',', ze, by, si, Y, '?'])
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
    ( 
        X @< Y -> merge(XS, [YScript|YS], S), R = [XScript|S];
        X = Y -> merge(XS, YS, S), R = [XScript|S];
        merge([XScript|XS], YS, S), R = [YScript|S]
    ).

merge_sort([], []) :- !.
merge_sort([X], [X]) :- !.
merge_sort(L, S) :-
  L = [_,_|_],
  halve(L, LL, LR),
  merge_sort(LL, LS),
  merge_sort(LR, RS),
  merge(LS, RS, S).