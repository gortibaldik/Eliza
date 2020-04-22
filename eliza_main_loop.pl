:- ensure_loaded(eliza_input).
:- ensure_loaded(eliza_output).
:- ensure_loaded(eliza_scripts).

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
    get_scripts_matching_keywords(User_input, Scripts),
    get_comment_(User_input, Scripts, Output, Keyword, Pattern_index),
    assert_next_action(Keyword, Pattern_index).

    
get_comment_(_, [], Output, Keyword, Pattern_index) :-
    get_initial_uninformed_comment(ec((Output,_,_),_,_), Keyword, Pattern_index).

get_comment_(User_input, [Script|Rest], Output, Keyword, Pattern_index) :-
    % at first we get uninformed response from Eliza
    % without any knowledge of User_input
    get_initial_uninformed_comment(Initial_comment, Initial_keyword, Initial_pattern_index),

    get_script_priority(Script, Priority),
    get_ec_keyword_priority(Initial_comment, Initial_priority),
    (
        Initial_priority > Priority ->  ( get_ec_comment(Initial_comment, Response), Initial_comment = ec((Output,_,_),_,_),
            Keyword = Initial_keyword, Pattern_index = Initial_pattern_index);
        get_informed_comment(Script, Informed_comment, Keyword, Pattern_index), Output = Informed_comment
    ),
    !.