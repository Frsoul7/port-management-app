:- module(heuristic_departure, [heuristic_early_departure_time/2]).
:- use_module(helpers).

heuristic_early_departure_time(SeqTripletsH, SDelaysH) :-
    get_time(Ti),
    findall((Departure,V), vessel(V,_,Departure,_,_), LDV),
    sort(LDV, LDVSorted),
    obtain_vessels(LDVSorted, SeqV),
    sequence_temporization(SeqV, SeqTripletsH),
    write('Heuristic Early Departure Sequence: '), write(SeqTripletsH), nl,
    sum_delays(SeqTripletsH, SDelaysH),
    get_time(Tf),
    T is Tf - Ti,
    write('Total Delay: '), write(SDelaysH), nl,
    format('Computation Time: ~6f seconds~n', [T]).