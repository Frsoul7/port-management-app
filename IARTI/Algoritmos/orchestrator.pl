:- module(orchestrator, [plan_vessels_auto/4]).

:- use_module(helpers).
:- use_module(heuristic_weighted_priority).
:- use_module(genetic_algorithm).

% ===========================================================================
% plan_vessels_auto(+MaxTime, +Vessels, -Result, -MetaData)
% Predicado principal: Orquestra a seleção, execução e formatação dos dados.
% ===========================================================================
plan_vessels_auto(MaxTime, Vessels, Result, MetaData) :-
    length(Vessels, N),

    % 1. POLÍTICA DE SELEÇÃO
    % O Prolog tentará as cláusulas de select_algorithm por ordem.
    select_algorithm(N, MaxTime, Algo),

    % 2. EXECUÇÃO E MEDIÇÃO
    get_time(T1),
    run_algo(Algo, Vessels, Seq, Delay),
    get_time(T2),

    % 3. CÁLCULO DE METADADOS
    Duration is T2 - T1,
    Result = _{sequence: Seq, total_delay: Delay},
    MetaData = _{
        algorithm_used: Algo,
        problem_size: N,
        computation_time: Duration,
        policy: "Automatic Selection Based on Size/Time"
    },

    % LOGS NO TERMINAL
    print_log(Algo, N, Duration).

% ===========================================================================
% Regras de Seleção (Substituem o IF)
% O uso do cut (!) garante que, ao encontrar a regra certa, não tenta as seguintes.
% ===========================================================================

% Caso 1: Problema pequeno e tempo suficiente -> Algoritmo Ótimo
select_algorithm(N, MaxTime, 'Optimal') :-
    N =< 10,
    MaxTime >= 30,
    !.

% Caso 2: Problema muito grande -> Algoritmo Genético
select_algorithm(N, _, 'Genetic') :-
    N > 25,
    !.

% Caso 3: Caso padrão (Default) -> Heurística
select_algorithm(_, _, 'Heuristic-WP').

% ===========================================================================
% Wrappers de Execução (run_algo)
% Polimorfismo: o Prolog escolhe a cláusula baseando-se no átomo 'Algo'.
% ===========================================================================

run_algo('Optimal', _, Seq, Delay) :-
    obtain_seq_shortest_delay_silent(Triplets, Delay),
    extract_names(Triplets, Seq).

run_algo('Heuristic-WP', _, Seq, Delay) :-
    heuristic_weighted_priority(Triplets, Delay),
    extract_names(Triplets, Seq).

run_algo('Genetic', Vessels, Seq, Delay) :-
    solve_genetic(Vessels, Seq, Delay).

% ===========================================================================
% Auxiliares
% ===========================================================================

% Extração de nomes de navios de uma lista de triplos (V, T1, T2)
extract_names([], []).
extract_names([(V,_,_)|T], [V|Names]) :-
    extract_names(T, Names).

% Helper para logging limpo
print_log(Algo, N, Duration) :-
    format('[LOG] Solucionador automático escolheu: ~w~n', [Algo]),
    format('[LOG] Navios: ~w | Tempo gasto: ~4fs~n', [N, Duration]).