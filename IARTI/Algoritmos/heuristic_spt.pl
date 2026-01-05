:- module(heuristic_spt, [heuristic_spt/2]).
:- use_module(helpers).

heuristic_spt(SeqTripletsH, SDelaysH) :-
    get_time(Ti),
    findall((ProcTime, V),(vessel(V, _, _, TUnload, TLoad),ProcTime is TUnload + TLoad),LProc),
    sort(LProc, LProcSorted),
    obtain_vessels(LProcSorted, SeqV),
    sequence_temporization(SeqV, SeqTripletsH),
    sum_delays(SeqTripletsH, SDelaysH),
    get_time(Tf),
    T is Tf - Ti,
    write('Heuristic SPT Sequence: '), write(SeqTripletsH), nl,
    write('Total Delay: '), write(SDelaysH), nl,
    format('Computation Time: ~6f seconds~n', [T]).
