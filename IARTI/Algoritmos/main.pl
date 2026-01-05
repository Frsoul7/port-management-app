:- use_module(data_vessels).
:- use_module(helpers).
:- use_module(heuristic_arrival).
:- use_module(heuristic_departure).
:- use_module(heuristic_spt).
:- use_module(heuristic_mst).
:- use_module(heuristic_weighted_priority).
:- use_module(multi_cranes).
:- use_module(api_server).
:- use_module(orchestrator).
:- use_module(genetic_algorithm).
:- use_module(benchmark).

% ===========================================================================
% RUN ALL: Executa todos os métodos para comparação manual
% ===========================================================================
run_all :-
    % 0. Preparar a lista de IDs de navios (necessário para o GA e Orchestrator)
    findall(V, vessel(V,_,_,_,_), LV),

    write('--- EARLY ARRIVAL ---'), nl,
    heuristic_early_arrival_time(SeqA, DelayA),
    format('Delay: ~w~n~n', [DelayA]),

    write('--- EARLY DEPARTURE ---'), nl,
    heuristic_early_departure_time(SeqD, DelayD),
    format('Delay: ~w~n~n', [DelayD]),

    write('--- SPT (Shortest Processing Time) ---'), nl,
    heuristic_spt(SeqSPT, DelaySPT),
    format('Delay: ~w~n~n', [DelaySPT]),

    write('--- MST (Minimum Slack Time) ---'), nl,
    heuristic_mst(SeqMST, DelayMST),
    format('Delay: ~w~n~n', [DelayMST]),

    write('--- WEIGHTED PRIORITY ---'), nl,
    heuristic_weighted_priority(SeqW, DelayW),
    format('Delay: ~w~n~n', [DelayW]),

    write('--- OPTIMAL SEARCH ---'), nl,
    obtain_seq_shortest_delay(SeqOpt, DelayOpt),
    format('Delay: ~w~n~n', [DelayOpt]),

    % --- ADIÇÃO: GENETIC ALGORITHM ---
    write('--- GENETIC ALGORITHM ---'), nl,
    solve_genetic(LV, SeqGA, DelayGA),
    format('Seq: ~w | Delay: ~w~n~n', [SeqGA, DelayGA]),

    % --- ADIÇÃO: AUTO ORCHESTRATOR ---
    write('--- AUTO ORCHESTRATOR (Decisão Automática) ---'), nl,
    % Exemplo: Limite de 10 segundos para decisão
    plan_vessels_auto(10, LV, Result, Meta),
    format('Resultado: ~w~n', [Result]),
    format('Meta: ~w~n~n', [Meta]),

    write('--- MULTI-CRANE OPTIMIZATION (Baseado no Optimal) ---'), nl,
    optimize_cranes(SeqOpt), nl.

% ===========================================================================
% START: Iniciar Servidor API
% ===========================================================================
start :-
    start_server(5000),
    format('Server running on port 5000~n').

% ===========================================================================
% TESTES E BENCHMARKS
% ===========================================================================

% Teste manual rápido para o Orchestrator
test_auto :-
    findall(V, vessel(V,_,_,_,_), LV),
    % Tenta planear com um limite de 60 segundos
    plan_vessels_auto(60, LV, Res, Meta),
    format('Resultado: ~w~n', [Res]),
    format('Meta: ~w~n', [Meta]).

run_benchmark :-
    Params = _{pop_size:80, generations:250, mutation_rate:0.15, time_limit:5.0},
    write('--- BENCHMARK SINGLE CRANE ---'), nl,
    benchmark_day(single, Params, R1),
    write(R1), nl,
    write('--- BENCHMARK MULTI CRANE ---'), nl,
    benchmark_day(multi, Params, R2),
    write(R2), nl.