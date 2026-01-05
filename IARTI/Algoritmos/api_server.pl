:- module(api_server, [start_server/1]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- use_module(data_vessels).
:- use_module(helpers).
:- use_module(heuristic_weighted_priority).
:- use_module(multi_cranes).
:- use_module(genetic_algorithm).
:- use_module(orchestrator).
:- use_module(benchmark).

% ---------------------------------------------------------------------------
% HTTP Handlers
% ---------------------------------------------------------------------------
:- http_handler('/weighted_priority', handle_weighted_priority, []).
:- http_handler('/optimal', handle_optimal, []).
:- http_handler('/multi_cranes', handle_multi_cranes, []).
:- http_handler('/genetic', handle_genetic, []).
:- http_handler('/benchmark', handle_benchmark, []).
:- http_handler(root(api/schedule), handle_request, []).

% ---------------------------------------------------------------------------
% Conversão para JSON (Helpers)
% ---------------------------------------------------------------------------

% Converte sequência normal [(v, s, e)]
sequence_to_json([], []).
sequence_to_json([(V, Start, End)|Rest], [_{vessel: VAtom, start: Start, end: End}|JsonRest]) :-
    atom_string(V, VAtom),
    sequence_to_json(Rest, JsonRest).

% Converte sequência com gruas [(v, s, e, c)]
sequence_with_cranes_to_json([], []).
sequence_with_cranes_to_json([(V, Start, End, Cranes)|Rest], [_{vessel: VAtom, start: Start, end: End, cranes: Cranes}|JsonRest]) :-
    atom_string(V, VAtom),
    sequence_with_cranes_to_json(Rest, JsonRest).

% Predicado polimórfico para escolher o conversor correto baseado na estrutura da lista
convert_result_to_json([], []).
convert_result_to_json([(_, _, _, _)|_] = Seq, Json) :- !, sequence_with_cranes_to_json(Seq, Json).
convert_result_to_json(Seq, Json) :- sequence_to_json(Seq, Json).

% ---------------------------------------------------------------------------
% Gestão de Dados Dinâmicos
% ---------------------------------------------------------------------------
populate_vessels([]).
populate_vessels([V|Vs]) :-
    assertz(vessel(V.name, V.arrival, V.departure, V.unload, V.load)),
    populate_vessels(Vs).

clear_vessels :-
    retractall(vessel(_,_,_,_,_)).

% ---------------------------------------------------------------------------
% Handlers de Algoritmos Individuais
% ---------------------------------------------------------------------------

handle_weighted_priority(Request) :-
    http_read_json(Request, JSON, [json_object(dict)]),
    clear_vessels,
    populate_vessels(JSON.vessels),
    heuristic_weighted_priority(Seq, Delay),
    convert_result_to_json(Seq, SeqJson),
    reply_json(_{sequence: SeqJson, total_delay: Delay}, [json_object(dict)]).

handle_optimal(Request) :-
    http_read_json(Request, JSON, [json_object(dict)]),
    clear_vessels,
    populate_vessels(JSON.vessels),
    obtain_seq_shortest_delay_silent(Seq, Delay),
    convert_result_to_json(Seq, SeqJson),
    reply_json(_{sequence: SeqJson, total_delay: Delay}, [json_object(dict)]).

handle_multi_cranes(Request) :-
    http_read_json(Request, JSON, [json_object(dict)]),
    clear_vessels,
    populate_vessels(JSON.vessels),
    obtain_seq_shortest_delay_silent(SeqOpt, _),
    optimize_cranes(SeqOpt, Result),
    convert_result_to_json(Result.sequence, SeqJson),
    FinalResult = _{
        sequence: SeqJson,
        total_delay: Result.total_delay,
        hours_with_2_cranes: Result.hours_with_2_cranes,
        total_cranes_needed: Result.total_cranes_needed
    },
    reply_json(FinalResult, [json_object(dict)]).

% ---------------------------------------------------------------------------
% Genetic Algorithm Handler (Refatorado sem IFs)
% ---------------------------------------------------------------------------
handle_genetic(Request) :-
    http_read_json(Request, JSON, [json_object(dict)]),
    clear_vessels,
    populate_vessels(JSON.vessels),
    findall(VId, vessel(VId,_,_,_,_), VIds),

    extract_mode(JSON, Mode),
    extract_params(JSON, Params),

    solve_genetic(VIds, Mode, Params, BestSeq, Metrics),
    sequence_temporization(BestSeq, Triplets),

    % Lógica multi-crane delegada a predicado auxiliar
    get_ga_final_sequence(Mode, Triplets, FinalSeqJson),
    reply_json(_{sequence: FinalSeqJson, metrics: Metrics}, [json_object(dict)]).

% Auxiliares para extrair dados do JSON sem usar IF
extract_mode(JSON, Mode) :- get_dict(mode, JSON, ModeStr), !, atom_string(Mode, ModeStr).
extract_mode(_, single).

extract_params(JSON, Params) :- get_dict(params, JSON, Params), !.
extract_params(_, _{}).

% Auxiliar para decidir a formatação final do GA
get_ga_final_sequence(multi, Triplets, SeqJson) :-
    optimize_cranes(Triplets, ResultCranes), !,
    sequence_with_cranes_to_json(ResultCranes.sequence, SeqJson).
get_ga_final_sequence(_, Triplets, SeqJson) :-
    sequence_to_json(Triplets, SeqJson).

% ---------------------------------------------------------------------------
% Orchestrator & Benchmark Handlers
% ---------------------------------------------------------------------------

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Servidor API em execucao na porta ~w~n', [Port]).

handle_request(Request) :-
    http_read_json_dict(Request, JSONIn),

    % Uso de .get(Key, Default) é aceitável, mas se quiser ser 100% puro:
    extract_time_limit(JSONIn, MaxTime),
    Vessels = JSONIn.get(vessels, []),

    plan_vessels_auto(MaxTime, Vessels, Plan, Meta),
    reply_json_dict(_{
        status: "success",
        plan: Plan,
        traceability: Meta
    }).

extract_time_limit(JSON, T) :- get_dict(time_limit, JSON, T), !.
extract_time_limit(_, 60).

handle_benchmark(Request) :-
    http_read_json(Request, JSON, [json_object(dict)]),
    clear_vessels,
    populate_vessels(JSON.vessels),

    extract_mode(JSON, Mode),
    extract_benchmark_params(JSON, Params),

    benchmark_day(Mode, Params, Results),
    reply_json(_{results:Results}, [json_object(dict)]).

extract_benchmark_params(JSON, P) :- get_dict(params, JSON, P), !.
extract_benchmark_params(_, _{pop_size:80, generations:250, mutation_rate:0.15, time_limit:5.0}).