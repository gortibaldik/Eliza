% list of all scripts used by Eliza to answer user prompts
% and utility functions to use them


% get_initial_uninformed_comment(-Initial_comment, -Keyword, -Pattern_index)
get_initial_uninformed_comment(Initial_comment, none, Pattern_index, Priority) :-
    Pattern_index = 0,
    Priority = -1,

    % we get initial uninformed comment from Eliza, from none_script
    get_action(none, Pattern_index, Action_index),
    none_script(None_script),
    get_script_pattern_by_index(None_script, Pattern_index, Pattern),
    get_pattern_actions(Pattern, Actions),

    % built-in
    % get nth element of Actions list and store it
    % into response(Comment)
    nth0(Action_index, Actions, response(Comment)),

    Initial_comment = Comment.


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
    findall(Script, (scripts(Script), script_contains_keyword(Script, Keyword), member_declined(Keyword, User_input)), MatchedScripts),
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
match(_, []) :-!.

match(Input, [VarHead|Pattern_tail]) :-
    var(VarHead), !,
    match_var(Input, Pattern_tail, VarHead, Rest, Pattern_rest),
    match(Rest, Pattern_rest).

match([Head|Tail], [Head|Pattern_tail]) :-
    !,match(Tail, Pattern_tail).

match([Head|Tail], [class(C, Head)|Pattern_tail]) :-
    call(C, Head), match(Tail, Pattern_tail).

% match_var(+Input, +Pattern, -Var, -Rest_of_input, -Rest_of_pattern)
%   match input to pattern until pattern and input head aren't 
%   the same or until you won't meet another variable
%   assign the result to Var
match_var([_|Rest], [Pattern_head|Pattern_rest], [], Rest, Pattern_rest) :-
    var(Pattern_head), !.

match_var([Head|Rest], [Head|Pattern_rest], [], Rest, Pattern_rest) :-!.

match_var([Head|Rest], [class(C, Head)|Pattern_tail], [], Rest, Pattern_tail) :-
    call(C, Head),!.

% having introduced scripts with keywords from classes 
% its not safe to call is_declination, because
% it calls atom_chars which throws exception on variables
% therefore we have to check, if Head isn't in form of class(class_name, Variable)
match_var([DeclinedHead|Rest], [Head|Pattern_rest], [], Rest, Pattern_rest) :-
    Head \= class(_,_),
    is_declination(DeclinedHead, Head), !.


% implicitly Pattern doesn't have same head
% as Input to be matched.
match_var([Head|Tail], Pattern, [Head|VarRest], Rest, Pattern_rest) :-
    match_var(Tail, Pattern, VarRest, Rest, Pattern_rest),!.

match_var(_, [], [], [], []).


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
                    response([myslite, ',', ze, je, pravdepodobne,',', ze, by, som, Y, '?']),
                    response([co, by, sa, stalo, ak, by, som, Y, '?']),
                    response([dufas, ze, by, som, Y, '?']),
                    response([naozaj, si, myslis, ',', ze, by, som, Y, '?'])
                ])
            ),
            pattern(
                matched([_, ak, by, som, Y]),
                actions([
                    response([co, by, sa, stalo, ak, by, ste, Y, '?']),
                    response([dufate, ',', ze, by, ste, Y, '?']),
                    response([preco, rozmyslate, ',', ze, by, ste, Y, '?'])
                ])
            )
        ]
    )
).

scripts(Script) :-
    family(Family), 
    Script = script(
        keyword(Family, 2),
        [
            pattern(
                matched([_, class(family_declined, F), X]),
                actions([
                    response([povedzte, mi, viac, o, vasej, rodine, '!']),
                    response([je, este, niekto, vo, vasej, rodine, kto, X, '?']),
                    response([myslite, vazne, ',', ked, hovorite, ',', ze, F, X, '?']),
                    response([F,'?']),
                    response([co, vam, este, prichadza, na, mysel, ',', ked, hovorite, ':', '\"', F, X, '\"'])
                ])
            )
        ]
     ).


scripts(
    script(
        keyword(co, 0),
        [
        pattern(
            matched([_]),
            actions([
            response([preco, sa, pytate, '?']),
            response([zaujima, vas, ta, otazka, '?']),
            response([co, je, to, ',', co, naozaj, chcete, vediet, ?]) ,
            response([casto, sa, zaoberate, takymito, otazkami, ?]),
            response([ktora, odpoved, by, sa, vam, najviac, pacila, ?]),
            response([co, si, myslite, ?]),
            response([co, vam, pride, na, um, ked, sa, toto, opytate, ?]),
            response([uz, ste, sa, nad, touto, otazkou, niekedy, zamyslali, ?]),
            response([uz, ste, sa, na, to, niekoho, pytali, ?])
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
        X @> Y -> merge(XS, [YScript|YS], S), R = [XScript|S];
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