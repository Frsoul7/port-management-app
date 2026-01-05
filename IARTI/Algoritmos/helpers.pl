:- module(helpers, [
    sequence_temporization/2,
    sum_delays/2,
    obtain_seq_shortest_delay/2,
    obtain_seq_shortest_delay_silent/2,
    obtain_vessels/2
]).

:- use_module(data_vessels).
:- dynamic shortest_delay/2.


sequence_temporization(LV, SeqTriplets) :-
    sequence_temporization1(0, LV, SeqTriplets).

sequence_temporization1(EndPrevSeq, [V|LV], [(V,TInUnload,TEndLoad)|SeqTriplets]) :-
    vessel(V,TIn,_,TUnload,TLoad),
    ((TIn > EndPrevSeq, !, TInUnload is TIn); TInUnload is EndPrevSeq + 1),
    TEndLoad is TInUnload + TUnload + TLoad - 1,
    sequence_temporization1(TEndLoad, LV, SeqTriplets).
sequence_temporization1(_, [], []).


sum_delays([], 0).
sum_delays([(V,_,TEndLoad)|LV], S) :-
    vessel(V,_,TDep,_,_),
    TPossibleDep is TEndLoad + 1,
    ((TPossibleDep > TDep, !, SV is TPossibleDep - TDep); SV is 0),
    sum_delays(LV, SLV),
    S is SV + SLV.


obtain_vessels([], []).
obtain_vessels([(_,V)|T], [V|Vs]) :-
    obtain_vessels(T, Vs).


obtain_seq_shortest_delay_silent(SeqBetterTriplets, SShortestDelay) :-
    retractall(shortest_delay(_,_)),
    asserta(shortest_delay(_, 100000)),
    (obtain_seq_shortest_delay1 ; true),
    retract(shortest_delay(SeqBetterTriplets, SShortestDelay)).


obtain_seq_shortest_delay(SeqBetterTriplets, SShortestDelay) :-
    get_time(Ti),
    (obtain_seq_shortest_delay1 ; true),
    retract(shortest_delay(SeqBetterTriplets, SShortestDelay)),
    write('Better Sequence: '), write(SeqBetterTriplets), nl,
    write('Shortest Delay: '), write(SShortestDelay), nl,
    get_time(Tf),
    T is Tf - Ti,
    format('Computation Time: ~6f seconds~n', [T]).



obtain_seq_shortest_delay1 :-
    asserta(shortest_delay(_, 100000)),
    findall(V, vessel(V,_,_,_,_), LV),
    permutation(LV, SeqV),
    sequence_temporization(SeqV, SeqTriplets),
    sum_delays(SeqTriplets, S),
    compare_shortest_delay(SeqTriplets, S),
    fail.

compare_shortest_delay(SeqTriplets, S) :-
    shortest_delay(_, SLower),
    (S < SLower -> retract(shortest_delay(_,_)), asserta(shortest_delay(SeqTriplets, S)) ; true).
