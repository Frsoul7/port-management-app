:- module(benchmark, [benchmark_day/3]).

:- use_module(data_vessels).
:- use_module(helpers).

:- use_module(heuristic_arrival).
:- use_module(heuristic_departure).
:- use_module(heuristic_spt).
:- use_module(heuristic_mst).
:- use_module(heuristic_weighted_priority).

:- use_module(genetic_algorithm).

/*
benchmark_day(+GAMode, +GAParams, -Results)

- GAMode: single | multi
- GAParams: dict com pop_size, generations, mutation_rate, time_limit
- Results: lista de dicts com {algorithm, total_delay, compute_time, extra}

Serve para cumprir: "Performance and result quality ... must be comparable against existing algorithms"
(US 4.3.1). :contentReference[oaicite:2]{index=2}
*/

benchmark_day(GAMode, GAParams, Results) :-
    % 1) Heurísticas (todas já existentes no vosso repo)
    timed_run('heuristic_arrival', heuristic_early_arrival_time(_SeqA, DelayA), DelayA, TA, R1),
    timed_run('heuristic_departure', heuristic_early_departure_time(_SeqD, DelayD), DelayD, TD, R2),
    timed_run('heuristic_spt', heuristic_spt(_SeqSPT, DelaySPT), DelaySPT, TSPT, R3),
    timed_run('heuristic_mst', heuristic_mst(_SeqMST, DelayMST), DelayMST, TMST, R4),
    timed_run('heuristic_weighted_priority', heuristic_weighted_priority(_SeqW, DelayW), DelayW, TW, R5),

    % 2) Optimal (silent) - só faz sentido para instâncias pequenas
    timed_run_optimal('optimal', TripletsOpt, DelayOpt, TOpt, R6),

    % 3) GA (single ou multi), com params configuráveis
    findall(V, vessel(V,_,_,_,_), Vessels),
    timed_run_ga(GAMode, GAParams, Vessels, R7),

    Results = [
        R1.put(_{total_delay:DelayA, compute_time:TA}),
        R2.put(_{total_delay:DelayD, compute_time:TD}),
        R3.put(_{total_delay:DelaySPT, compute_time:TSPT}),
        R4.put(_{total_delay:DelayMST, compute_time:TMST}),
        R5.put(_{total_delay:DelayW, compute_time:TW}),
        R6.put(_{total_delay:DelayOpt, compute_time:TOpt, extra:_{sequence_triplets:TripletsOpt}}),
        R7
    ].

timed_run(Name, Goal, Delay, Time, Result) :-
    get_time(T0),
    call(Goal),
    get_time(T1),
    Time is T1 - T0,
    Result = _{algorithm:Name}.

timed_run_optimal(Name, Triplets, Delay, Time, Result) :-
    get_time(T0),
    obtain_seq_shortest_delay_silent(Triplets, Delay),
    get_time(T1),
    Time is T1 - T0,
    Result = _{algorithm:Name}.

timed_run_ga(Mode, Params, Vessels, Result) :-
    get_time(T0),
    solve_genetic(Vessels, Mode, Params, _BestSeq, Metrics),
    get_time(T1),
    Time is T1 - T0,
    Result = _{
        algorithm:genetic,
        mode:Mode,
        total_delay:Metrics.total_delay,
        compute_time:Time,
        extra:_{fitness:Metrics.fitness, hours_with_2_cranes:Metrics.hours_with_2_cranes}
    }.
