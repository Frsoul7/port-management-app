:- module(genetic_algorithm, [
    solve_genetic/3,
    solve_genetic/5
]).

:- use_module(helpers).
:- use_module(multi_cranes).
:- use_module(library(random)).

% -----------------------------
% Defaults / backward-compat
% -----------------------------

default_params(_{
    pop_size: 60,
    generations: 200,
    mutation_rate: 0.15,
    time_limit: 5.0,
    alpha_hours2: 0.1
}).

solve_genetic(Vessels, BestSeq, TotalDelay) :-
    default_params(P),
    solve_genetic(Vessels, single, P, BestSeq, M),
    TotalDelay = M.total_delay.

solve_genetic(Vessels, Mode, ParamsIn, BestSeq, Metrics) :-
    default_params(Defaults),
    merge_params(Defaults, ParamsIn, Params),
    get_time(T0),
    P = Params.pop_size,
    generate_pop(P, Mode, Params, Vessels, Pop),
    G = Params.generations,
    evolve(G, Mode, Params, T0, Pop, FinalPop),
    best_of_population(Mode, Params, FinalPop, BestSeq, BestFitness, BestDelay, BestH2),
    get_time(T1),
    Duration is T1 - T0,
    Metrics = _{
        fitness: BestFitness,
        total_delay: BestDelay,
        hours_with_2_cranes: BestH2,
        compute_time: Duration,
        mode: Mode,
        pop_size: Params.pop_size,
        generations: Params.generations,
        mutation_rate: Params.mutation_rate,
        time_limit: Params.time_limit
    }.

% Remoção do IF em merge_params
merge_params(Defaults, ParamsIn, ParamsOut) :-
    is_dict(ParamsIn), !,
    dict_pairs(Defaults, Tag, PairsD),
    dict_pairs(ParamsIn, Tag, PairsIn),
    merge_pairs(PairsD, PairsIn, PairsOut),
    dict_pairs(ParamsOut, Tag, PairsOut).
merge_params(Defaults, _, Defaults).

merge_pairs([], _, []).
merge_pairs([K-V|T], In, [K-VFinal|OutT]) :-
    (member(K-VIn, In) -> VFinal = VIn ; VFinal = V), % Member aqui é aceitável, mas para ser rigoroso:
    merge_pairs(T, In, OutT).

% -----------------------------
% Population generation
% -----------------------------

generate_pop(0, _, _, _, []) :- !.
generate_pop(N, Mode, Params, Vessels, [ind(Seq, Fit, Delay, H2)|Rest]) :-
    random_permutation(Vessels, Seq),
    evaluate(Mode, Params, Seq, Fit, Delay, H2),
    N1 is N - 1,
    generate_pop(N1, Mode, Params, Vessels, Rest).

% -----------------------------
% Evolution loop
% -----------------------------

evolve(0, _, _, _, Pop, Pop) :- !.
evolve(G, Mode, Params, T0, Pop, Out) :-
    get_time(Now),
    Elapsed is Now - T0,
    check_evolution_status(Elapsed, G, Mode, Params, T0, Pop, Out).

% Condição de parada: Tempo esgotado
check_evolution_status(Elapsed, _, _, Params, _, Pop, Pop) :-
    Elapsed > Params.time_limit, !.
% Condição de continuação
check_evolution_status(_, G, Mode, Params, T0, Pop, Out) :-
    length(Pop, L),
    Keep0 is max(1, L // 10),
    sort_pop(Pop, Sorted),
    take(Keep0, Sorted, Elite),
    Need is L - Keep0,
    create_offspring(Need, Mode, Params, Sorted, Children),
    append(Elite, Children, Next),
    G1 is G - 1,
    evolve(G1, Mode, Params, T0, Next, Out).

create_offspring(0, _, _, _, []) :- !.
create_offspring(N, Mode, Params, Pop, [ind(Child, Fit, Delay, H2)|Rest]) :-
    tournament_select(Mode, Params, Pop, ind(P1,_,_,_)),
    tournament_select(Mode, Params, Pop, ind(P2,_,_,_)),
    crossover_ox(P1, P2, C0),
    mutate_swap(Params.mutation_rate, C0, Child),
    evaluate(Mode, Params, Child, Fit, Delay, H2),
    N1 is N - 1,
    create_offspring(N1, Mode, Params, Pop, Rest).

% -----------------------------
% Selection
% -----------------------------

tournament_select(_Mode, _Params, Pop, Winner) :-
    random_member(A, Pop),
    random_member(B, Pop),
    random_member(C, Pop),
    better(A, B, W1),
    better(W1, C, Winner).

% Melhor fitness (Minimização)
better(ind(S1,F1,D1,H1), ind(_,F2,_,_), ind(S1,F1,D1,H1)) :- F1 < F2, !.
% Empate de fitness, desempate por delay
better(ind(S1,F1,D1,H1), ind(_,F1,D2,_,), ind(S1,F1,D1,H1)) :- D1 < D2, !.
% Empate de fitness e delay, desempate por H2
better(ind(S1,F1,D1,H1), ind(_,F1,D1,H2), ind(S1,F1,D1,H1)) :- H1 =< H2, !.
% Caso contrário, o segundo é melhor
better(_, B, B).

best_of_population(_Mode, _Params, [ind(S,F,D,H)|T], BestS, BestF, BestD, BestH) :-
    foldl(best_fold, T, ind(S,F,D,H), ind(BestS,BestF,BestD,BestH)).

best_fold(X, Acc, Best) :- better(X, Acc, Best).

% -----------------------------
% Crossover (OX)
% -----------------------------

crossover_ox(P1, _, P1) :- length(P1, L), L < 2, !.
crossover_ox(P1, P2, Child) :-
    length(P1, L),
    random_between(1, L, A),
    random_between(1, L, B),
    Min is min(A,B),
    Max is max(A,B),
    slice(P1, Min, Max, Slice),
    exclude(member_of(Slice), P2, Rest),
    PrefixLen is Min - 1,
    prefix_len(PrefixLen, Rest, Prefix, Suffix),
    append(Prefix, Slice, T),
    append(T, Suffix, Child).

member_of(L, X) :- member(X, L).

slice(L, I, J, Out) :-
    findall(E, (nth1(K, L, E), K >= I, K =< J), Out).

prefix_len(0, L, [], L) :- !.
prefix_len(N, [H|T], [H|R], S) :-
    N1 is N - 1,
    prefix_len(N1, T, R, S).

% -----------------------------
% Mutation
% -----------------------------

mutate_swap(Rate, Seq, Mutated) :-
    random_float(R),
    check_mutation(R, Rate, Seq, Mutated).

% Não muta
check_mutation(R, Rate, Seq, Seq) :- R >= Rate, !.
% Tenta mutar, mas sequência muito curta
check_mutation(_, _, Seq, Seq) :- length(Seq, L), L < 2, !.
% Efetua a mutação
check_mutation(_, _, Seq, Mutated) :-
    length(Seq, L),
    random_between(1, L, I1),
    random_between(1, L, I2),
    perform_swap(I1, I2, Seq, Mutated).

perform_swap(I, I, Seq, Seq) :- !.
perform_swap(I1, I2, Seq, Mutated) :-
    swap_positions(I1, I2, Seq, Mutated).

swap_positions(I1, I2, Seq, Out) :-
    nth1(I1, Seq, E1),
    nth1(I2, Seq, E2),
    set_nth1(I1, Seq, E2, T),
    set_nth1(I2, T, E1, Out).

set_nth1(1, [_|T], X, [X|T]) :- !.
set_nth1(N, [H|T], X, [H|R]) :-
    N1 is N - 1,
    set_nth1(N1, T, X, R).

% -----------------------------
% Fitness / Evaluation
% -----------------------------

evaluate(single, _Params, Seq, Fit, Delay, 0) :-
    sequence_temporization(Seq, Triplets),
    sum_delays(Triplets, Delay),
    Fit is float(Delay).

evaluate(multi, Params, Seq, Fit, Delay, Hours2) :-
    sequence_temporization(Seq, Triplets),
    sum_delays(Triplets, BaseDelay),
    multi_crane_logic(BaseDelay, Triplets, Params, Fit, Delay, Hours2).

% Se delay base já é 0, não precisa de otimização de guindastes
multi_crane_logic(0, _, _, 0.0, 0, 0) :- !.
% Tenta otimizar com multi-cranes
multi_crane_logic(_, Triplets, Params, Fit, Delay, Hours2) :-
    optimize_cranes(Triplets, Result), !,
    Delay = Result.total_delay,
    Hours2 = Result.hours_with_2_cranes,
    Fit is Delay + Params.alpha_hours2 * Hours2.
% Se a otimização falhar, usa o delay base
multi_crane_logic(BaseDelay, _, _, Fit, BaseDelay, 0) :-
    Fit is float(BaseDelay).

% -----------------------------
% Utilities
% -----------------------------

sort_pop(Pop, Sorted) :-
    predsort(compare_ind, Pop, Sorted).

compare_ind(<, ind(_,F1,_,_), ind(_,F2,_,_)) :- F1 < F2, !.
compare_ind(>, ind(_,F1,_,_), ind(_,F2,_,_)) :- F1 > F2, !.
compare_ind(=, _, _).

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|R]) :-
    N1 is N - 1,
    take(N1, T, R).