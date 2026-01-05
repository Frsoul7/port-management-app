:- module(multi_cranes, [
    optimize_cranes/1,
    optimize_cranes/2
]).

:- use_module(helpers).  
:- dynamic best_crane_combination/3.


optimize_cranes(SeqTriplets) :-
    retractall(best_crane_combination(_,_,_)),
    asserta(best_crane_combination(_, 100000, 100000)),  
    find_last_delayed_vessel(SeqTriplets, LastDelayedIndex),
    generate_crane_combinations(SeqTriplets, 1, LastDelayedIndex, [], _NewSeq),
    best_crane_combination(BestSeq, 0, Hours2Cranes),
    write('Nova Sequencia: '), write(BestSeq), nl,
    write('Total Delay: 0'), nl,
    write('Horas com 2 Cranes: '), write(Hours2Cranes), nl.


optimize_cranes(SeqTriplets, Result) :-
    retractall(best_crane_combination(_,_,_)),
    asserta(best_crane_combination(_, 100000, 100000)),  
    find_last_delayed_vessel(SeqTriplets, LastDelayedIndex),
    generate_crane_combinations(SeqTriplets, 1, LastDelayedIndex, [], _NewSeq),
    best_crane_combination(BestSeq, 0, Hours2Cranes),

        Result = _{
            sequence: BestSeq,
            total_delay: 0,
            hours_with_2_cranes: Hours2Cranes,
            total_cranes_needed: 2
        }.
    

find_last_delayed_vessel(SeqTriplets, Index) :-
    findall(I, (
        nth1(I, SeqTriplets, (V, _, TEndLoad)),
        vessel(V, _, TDep, _, _),
        TEndLoad + 1 > TDep
    ), DelayedIndices),
    max_list(DelayedIndices, Index).


generate_crane_combinations(_, CurrentIndex, LastIndex, Acc, Acc) :-
    CurrentIndex > LastIndex,
    evaluate_crane_sequence(Acc).

generate_crane_combinations(SeqTriplets, CurrentIndex, LastIndex, Acc, Result) :-
    CurrentIndex =< LastIndex,
    nth1(CurrentIndex, SeqTriplets, (V, TInUnload, _)),
    vessel(V, _, _, Unload, Load),
    ProcessingTime is Unload + Load,
    
    TEnd1 is TInUnload + ProcessingTime - 1,
    NextIndex is CurrentIndex + 1,
    generate_crane_combinations(SeqTriplets, NextIndex, LastIndex,
        [(V, TInUnload, TEnd1, 1)|Acc], Result).

generate_crane_combinations(SeqTriplets, CurrentIndex, LastIndex, Acc, Result) :-
    CurrentIndex =< LastIndex,
    nth1(CurrentIndex, SeqTriplets, (V, TInUnload, _)),
    vessel(V, _, _, Unload, Load),
    ProcessingTime is Unload + Load,
    
    TEnd2 is TInUnload + ceiling(ProcessingTime / 2) - 1,
    NextIndex is CurrentIndex + 1,
    generate_crane_combinations(SeqTriplets, NextIndex, LastIndex,
        [(V, TInUnload, TEnd2, 2)|Acc], Result).


evaluate_crane_sequence(SeqWithCranes) :-
    reverse(SeqWithCranes, Seq),  
    sum_delays_with_cranes(Seq, TotalDelay, Hours2Cranes),
    ( TotalDelay =:= 0 ->
        best_crane_combination(_, _, CurrentHours),
        (Hours2Cranes < CurrentHours ->
            retractall(best_crane_combination(_,_,_)),
            asserta(best_crane_combination(Seq, TotalDelay, Hours2Cranes))
        ; true
        )
    ; true
    ).

sum_delays_with_cranes([], 0, 0).
sum_delays_with_cranes([(V, TInUnload, TEndLoad, Cranes)|Rest], TotalDelay, Hours2) :-
    vessel(V, _, TDep, _, _),
    Delay is max(0, (TEndLoad + 1) - TDep),
    sum_hours_2_cranes(Cranes, TInUnload, TEndLoad, Hours2This),
    sum_delays_with_cranes(Rest, RestDelay, RestHours2),
    TotalDelay is Delay + RestDelay,
    Hours2 is Hours2This + RestHours2.


sum_hours_2_cranes(Cranes, TInUnload, TEndLoad, Hours2) :-
    (Cranes =:= 2 ->
        Hours2 is TEndLoad - TInUnload + 1
    ;
        Hours2 is 0
    ).
