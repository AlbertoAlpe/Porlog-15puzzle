:- module(regole, [adjacent/3, adjacent/4, tilezero/2, trasforma/3, actions/2, testabili/2, choose_hmin/3, action_sort/2, strip_hval/2, azioni/3, azioni_valide/4]).

side(4).

% adjacent/3 - adjacent(+Config, +Tilef, -Tiles)
adjacent(Config, TileF, Tiles) :-
    findall(T, adjacent(Config, TileF, T, _), Tiles).

%adjacent/4 - adjacent(config, tile_1, tile_2, direction)
% true se le tile_1 ha tile_2 alla propria direction.
adjacent(Config, tile(X1, Y1, N1), tile(X2, Y1, N2), left) :- 
    member(tile(X1, Y1, N1), Config),
    member(tile(X2, Y1, N2), Config),
    X2 is X1-1,
    X2 \== 0,
    N1 \== N2.

adjacent(Config, tile(X1, Y1, N1), tile(X2, Y1, N2), right) :- 
    member(tile(X1, Y1, N1), Config),
    member(tile(X2, Y1, N2), Config),
    X2 is X1+1,
    side(S),
    X2 \== S+1,
    N1 \== N2.

adjacent(Config, tile(X1, Y1, N1), tile(X1, Y2, N2), up) :- 
    member(tile(X1, Y1, N1), Config),
    member(tile(X1, Y2, N2), Config),
    Y2 is Y1-1,
    Y2 \== 0,
    N1 \== N2.

adjacent(Config, tile(X1, Y1, N1), tile(X1, Y2, N2), down) :- 
    member(tile(X1, Y1, N1), Config),
    member(tile(X1, Y2, N2), Config),
    Y2 is Y1+1,
    side(S),
    Y2 \== S+1,
    N1 \== N2.

%tilezero/2 - data una configurazione restituisce la posizione della tile vuota (valore zero)
tilezero(Config, tile(X, Y, 0)) :- member(tile(X, Y, 0), Config), !.

% tile_swap(tile(X, Y, Z), tile(I, J, K), Config, NewConfig)
tile_swap(_, _, [], []).
tile_swap(tile(X, Y, Z), tile(I, J, K), [tile(X, Y, Z)|Tail], [tile(X, Y, K)|Tail2]) :-
    tile_swap(tile(X, Y, Z), tile(I, J, K), Tail, Tail2), !.
tile_swap(tile(X, Y, Z), tile(I, J, K), [tile(I, J, K)|Tail], [tile(I, J, Z)|Tail2]) :-
    tile_swap(tile(X, Y, Z), tile(I, J, K), Tail, Tail2), !.
tile_swap(tile(X, Y, Z), tile(I, J, K), [Different|Tail], [Different|Tail2]) :-
    !, Different \== tile(X, Y, Z), !, tile_swap(tile(X, Y, Z), tile(I, J, K), Tail, Tail2).
tile_swap(tile(X, Y, Z), tile(I, J, K), [Different|Tail], [Different|Tail2]) :-
    !, Different \== tile(I, J, K), !, tile_swap(tile(X, Y, Z), tile(I, J, K), Tail, Tail2).

% trasforma(+(Tile, Action), +S, -SNuovo)
% applica una determinata azione a una configurazione
trasforma((Tile, Action), Config, NuovaConfig) :- 
                        member(tile(_, _, _), Config),          
                        tilezero(Config, TileZ),            
                        adjacent(Config, Tile, TileZ, Action), 
                        !,
                        tile_swap(TileZ, Tile, Config, NuovaConfig).

% actions/2 - actions(+Config, -ActionList)
% trova le azioni applicabili data una configurazione
actions([], []).
actions(Config, Actions) :-
    tilezero(Config, TileZ),
    findall((Tile, Action), adjacent(Config, Tile, TileZ, Action), Actions).

%trova le azioni possibili e le ordina in base all'euristica risultante
testabili(Config, Stripped):-
    % Trova tutte le possibili azioni per questa configurazione
    actions(Config, Actions),
    % Calcola l_euristica per tutte le azioni testabili. Associa ad ogni action(Tile, Action) il corrispondente valore di euristica
    choose_hmin(Config, Actions, Valued),
    % Ordina le azioni in base al valore dell euristica crescente
    action_sort(Valued, Sorted),
    % Rimuove il valore dell_euristica e restituisce la lista di azioni in Stripped
    strip_hval(Sorted, Stripped), !.

choose_hmin(_, [], [hval(none, 9999)]).

choose_hmin(Config, [Azione|AzioniTail], [hval(Azione, NextF)|Hvals]) :-
    trasforma(Azione, Config, NuovaConfig),
    heuristics: f(0, NuovaConfig, NextF),
    choose_hmin(Config, AzioniTail, Hvals).

% Ordina le azioni in base la valore di euristica crescente
action_sort([hval(A, V), hval(A1, V1)], [hval(A1, V1), hval(A, V)]) :-
    V > V1.
action_sort([hval(A, V), hval(A1, V1)], [hval(A, V), hval(A1, V1)]) :-
    V =< V1.
action_sort([hval(A, V)|Hvals], Sorted) :- 
    action_sort(Hvals, [hval(A1, V1)|SortedT]),
    action_sort([hval(A, V), hval(A1, V1)], SortedN),
    append(SortedN, SortedT, Sorted).

% Rimuove i valori delle euristiche dalla lista di azioni
strip_hval([hval(none, 9999)], []).
strip_hval([hval(A, _)|Actions], [A|Stripped]) :- strip_hval(Actions, Stripped).

% ------Queste ultime due regole(azioni e azioni_valide) non sono utilizzate------
% trova tutte le azioni per una Config, poi seleziona solo quelle che portano a stati non visitati
azioni([], _, []).

azioni(Config, Visited, Actionsok) :-
    tilezero(Config, TileZ),
    findall((Tile, Action), adjacent(Config, Tile, TileZ, Action), AllActions), %qui ho tutte le azioni possibili
    azioni_valide(Config, AllActions, Visited, Actionsok).

 
azioni_valide(_, [], _, []).

azioni_valide(Config, [(Tile, Action)|ActionsTail], Visited, [(Tile, Action)|Actionsok]):-    
    trasforma((Tile, Action), Config, NuovaConfig),
    \+member(NuovaConfig, Visited),
    azioni_valide(Config, ActionsTail, Visited, Actionsok).

azioni_valide(Config, [_|ActionsTail], Visited, Actionsok):-
    azioni_valide(Config, ActionsTail, Visited, Actionsok).