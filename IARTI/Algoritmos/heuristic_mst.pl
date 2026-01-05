:- module(heuristic_mst, [heuristic_mst/2]).
:- use_module(helpers).

heuristic_mst(SeqTripletsH, SDelaysH) :-
    get_time(Ti),
    findall((Slack, V),(vessel(V, Arrival, Departure, TUnload, TLoad), Slack is Departure - (Arrival + TUnload + TLoad)), LSlack),
    sort(LSlack, LSlackSorted),
    obtain_vessels(LSlackSorted, SeqV),
    sequence_temporization(SeqV, SeqTripletsH),
    sum_delays(SeqTripletsH, SDelaysH),
    get_time(Tf),
    T is Tf - Ti,
    write('Heuristic MST Sequence: '), write(SeqTripletsH), nl,
    write('Total Delay: '), write(SDelaysH), nl,
    format('Computation Time: ~6f seconds~n', [T]).
