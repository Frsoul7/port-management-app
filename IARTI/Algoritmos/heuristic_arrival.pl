:- module(heuristic_arrival, [heuristic_early_arrival_time/2]).
:- use_module(helpers).

heuristic_early_arrival_time(SeqTripletsH, SDelaysH) :-
    get_time(Ti),
    findall((Arrival,V), vessel(V,Arrival,_,_,_), LAV),
    sort(LAV, LAVSorted),
    obtain_vessels(LAVSorted, SeqV),
    sequence_temporization(SeqV, SeqTripletsH),
    sum_delays(SeqTripletsH, SDelaysH),
    get_time(Tf),
    T is Tf - Ti,
    write('Heuristic Early Arrival Sequence: '), write(SeqTripletsH), nl,
    write('Total Delay: '), write(SDelaysH), nl,
    format('Computation Time: ~6f seconds~n', [T]).
