:- module(heuristic_weighted, [heuristic_weighted_priority/2]).
:- use_module(helpers).
:- use_module(data_vessels).

% Weighted Priority Heuristic
% Priority = w1*SlackTime + w2*ProcessingTime + w3*ArrivalTime
% Weights: 40% slack, 30% processing, 30% arrival

heuristic_weighted_priority(SeqTripletsH, SDelaysH) :-
    statistics(walltime, [Ti|_]),
    findall((Priority, V), calculate_priority(V, Priority), LPV),
    sort(LPV, LPVSorted),
    obtain_vessels(LPVSorted, SeqV),
    sequence_temporization(SeqV, SeqTripletsH),
    sum_delays(SeqTripletsH, SDelaysH),
    statistics(walltime, [Tf|_]),
    T is (Tf - Ti),
    write('Heuristic Weighted Priority Sequence: '), write(SeqTripletsH), nl,
    write('Total Delay: '), write(SDelaysH), nl,
    format('Computation Time: ~6f seconds~n', [T]).

calculate_priority(V, Priority) :-
    vessel(V, Arrival, Departure, Unload, Load),
    ProcessingTime is Unload + Load,
    SlackTime is Departure - Arrival - ProcessingTime,
    Priority is 0.4 * SlackTime + 0.3 * ProcessingTime + 0.3 * Arrival.