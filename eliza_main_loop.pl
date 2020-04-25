:- ensure_loaded(eliza_input).
:- ensure_loaded(eliza_output).
:- ensure_loaded(eliza_scripts).
:- ensure_loaded(eliza_memory_patterns).

main_prompt :-
    start_script(Sentence),
    comment(Sentence),
    repeat,
        write('> '),
        read_atomics(User_input),
        (
            User_input = [dovidenia] -> quit_script(Output), comment(Output), true;
            User_input = [] -> fail;
            get_comment(User_input, Output), comment(Output), fail
        ),
    !.

get_comment(User_input, Output) :-
    traverse_input_stem_lemm(User_input, Stemmed_lemmed),

    get_scripts_matching_keywords(Stemmed_lemmed, Scripts),

    % after getting scripts with matching comments, we process
    % input further to get comment
    get_comment_(Stemmed_lemmed, Scripts, Output, Keyword, Pattern_index),

    % Keyword = vas switches on memory handling
    % if we've encountered "vas" then we keep in
    % head possible questions connected with this keyword
    (
        Keyword = vas ->
        get_random_memory_pattern(Pattern), 
        Pattern = memory_pattern(matched(Matched), Response),
        match(Stemmed_lemmed, Matched),
        append_to_memory_list(Response)
        ; true
    ), 
    prepare_new_comment(Keyword, Pattern_index).

    
get_comment_(_, [], Output, Keyword, Pattern_index) :-
    get_initial_uninformed_memory_comment(Output, Keyword, Pattern_index, _),!;
    get_initial_uninformed_comment(Output, Keyword, Pattern_index, _), !.

get_comment_(User_input, Scripts, Output, Keyword, Pattern_index) :-
    % at first we get uninformed response from Eliza
    % without any knowledge of User_input
    (get_initial_uninformed_memory_comment(IC, IK, IPI,IPR), !;
    get_initial_uninformed_comment(IC, IK, IPI, IPR)),

    % then we traverse all the scripts to find the best
    find_best_from_scripts(User_input, Scripts, (IC, IK, IPI, IPR), Output, Keyword, Pattern_index).

find_best_from_scripts(User_input, [Script|Rest], (IC, IK, IPI, IPR), Output, Keyword, Pattern_index) :-
    get_script_priority(Script, Informed_priority),
    (
        IPR > Informed_priority -> (Output = IC, Keyword = IK, Pattern_index = IPI)
        ;
        get_informed_comment(User_input, Script, Action, Informed_Keyword, Informed_index) ->
            (
                Action = newkey ->
                find_best_from_scripts(User_input, Rest, (IC, IK, IPI, IPR), Output, Keyword, Pattern_index)
                ;
                Action = equivalence(NewKeyword) ->
                prepare_new_comment(Informed_Keyword, Informed_index),
                script_contains_keyword(NewScript, NewKeyword),
                find_best_from_scripts(User_input, [NewScript|Rest], (IC, IK, IPI, IPR), Output, Keyword, Pattern_index)
                ;
                Action = response(Tmp) ->
                flatten(Tmp, Informed_comment),!,
                find_best_from_scripts(User_input, Rest, (Informed_comment, Informed_Keyword, Informed_index, Informed_priority), Output, Keyword, Pattern_index)
            )
        ;
        find_best_from_scripts(User_input, Rest, (IC, IK, IPI, IPR), Output, Keyword, Pattern_index)
    ).

% base case at the end of script search
find_best_from_scripts(_, [], (IC,IK, IPI, _), IC, IK, IPI).

prepare_new_comment(Keyword, Pattern_index) :-
    Keyword = memory ->
    remove_head_memory_list
    ;
    assert_next_action(Keyword, Pattern_index),!.