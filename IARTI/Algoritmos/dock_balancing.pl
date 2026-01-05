:- module(dock_balancing, [
    balance_vessels_by_docks/2,
    test_balance/0
]).

% DATA
vessel(v01, 0, 6, 2, 2).
vessel(v02, 2, 8, 3, 2).
vessel(v03, 4, 10, 2, 3).
vessel(v04, 6, 12, 4, 2).
vessel(v05, 7, 14, 3, 3).
vessel(v06, 9, 16, 2, 2).
vessel(v07, 11, 18, 5, 3).
vessel(v08, 12, 19, 2, 2).
vessel(v09, 14, 21, 3, 2).
vessel(v10, 15, 22, 4, 3).
vessel(v11, 17, 23, 2, 2).
vessel(v12, 18, 24, 3, 2).
vessel(v13, 19, 24, 2, 2).
vessel(v14, 21, 24, 2, 1).
vessel(v15, 22, 24, 1, 1).

% DOCKS
dock(d1, 1).
dock(d2, 1).
dock(d3, 2).
dock(d4, 4).

% LOAD
calculate_load(VesselID, NumCranes, Load) :-
    vessel(VesselID, _, _, Tload, Tunload),
    TotalTime is Tload + Tunload,
    Load is ceiling(TotalTime / NumCranes).

% SORT
sort_vessels_by_arrival(Vessels, Sorted) :-
    findall(Arr-V, (member(V, Vessels), vessel(V, Arr, _, _, _)), Pairs),
    keysort(Pairs, SortedPairs),
    findall(V, member(_-V, SortedPairs), Sorted).

% BALANCE
balance_vessels_by_docks(Assignments, FinalLoads) :-
    findall(V, vessel(V, _, _, _, _), Vessels),
    sort_vessels_by_arrival(Vessels, Sorted),
    findall(dock(D, N), dock(D, N), Docks),
    findall(D-0, dock(D, _), InitialLoads),
    assign_vessels_to_docks(Sorted, Docks, InitialLoads, [], Assignments, FinalLoads).

assign_vessels_to_docks([], _, Loads, Acc, Assignments, Loads) :-
    reverse(Acc, Assignments).

assign_vessels_to_docks([V|Rest], Docks, CurrentLoads, Acc, Assignments, FinalLoads) :-
    find_least_loaded_dock(V, Docks, CurrentLoads, BestDock, LoadToAdd),
    update_dock_load(BestDock, LoadToAdd, CurrentLoads, NewLoads),
    NewAssignment = assignment(V, BestDock, LoadToAdd),
    assign_vessels_to_docks(Rest, Docks, NewLoads, [NewAssignment|Acc], Assignments, FinalLoads).

find_least_loaded_dock(VesselID, Docks, Loads, BestDock, LoadToAdd) :-
    findall(TotalLoad-(D-AddLoad), (
        member(dock(D, N), Docks),
        member(D-CurrentLoad, Loads),
        calculate_load(VesselID, N, AddLoad),
        TotalLoad is CurrentLoad + AddLoad
    ), Options),
    keysort(Options, [_-(BestDock-LoadToAdd)|_]).

update_dock_load(DockID, LoadToAdd, Loads, NewLoads) :-
    update_dock_load_helper(DockID, LoadToAdd, Loads, NewLoads).

update_dock_load_helper(_, _, [], []).
update_dock_load_helper(DockID, LoadToAdd, [DockID-CurLoad|Rest], [DockID-NewLoad|NewRest]) :-
    !,
    NewLoad is CurLoad + LoadToAdd,
    update_dock_load_helper(DockID, LoadToAdd, Rest, NewRest).
update_dock_load_helper(DockID, LoadToAdd, [D-Load|Rest], [D-Load|NewRest]) :-
    update_dock_load_helper(DockID, LoadToAdd, Rest, NewRest).

% OUTPUT
print_results(Assignments, FinalLoads) :-
    nl,
    write('========================================'), nl,
    write('  DISTRIBUICAO DE BARCOS POR DOCA      '), nl,
    write('  (Criterio: Ordem de Chegada)         '), nl,
    write('========================================'), nl, nl,
    
    findall(D, dock(D, _), AllDocks),
    forall(member(D, AllDocks), print_dock_assignments(D, Assignments, FinalLoads)),
    
    nl,
    write('========================================'), nl,
    write('           RESUMO DE CARGAS             '), nl,
    write('========================================'), nl,
    forall(member(D-Load, FinalLoads), (
        dock(D, Cranes),
        format('  ~w (~w grua(s)): CARGA TOTAL = ~wh~n', [D, Cranes, Load])
    )),
    
    findall(Load, member(_-Load, FinalLoads), AllLoads),
    sum_list(AllLoads, TotalGlobalLoad),
    format('~n  >>> CARGA TOTAL GLOBAL: ~wh <<<~n', [TotalGlobalLoad]),
    nl.

print_dock_assignments(DockID, Assignments, FinalLoads) :-
    findall(V, member(assignment(V, DockID, _), Assignments), VesselsInDock),
    length(VesselsInDock, NumVessels),
    member(DockID-TotalLoad, FinalLoads),
    dock(DockID, Cranes),
    
    format('~w (~w grua(s)) - ~w barcos - CARGA: ~wh~n', [DockID, Cranes, NumVessels, TotalLoad]),
    write('  Barcos: '),
    print_vessel_list(VesselsInDock),
    nl.

print_vessel_list([]).
print_vessel_list([V]) :- 
    !, 
    format('~w', [V]).
print_vessel_list([V|Rest]) :- 
    format('~w, ', [V]), 
    print_vessel_list(Rest).

% TEST
test_balance :-
    balance_vessels_by_docks(Assignments, Loads),
    print_results(Assignments, Loads).