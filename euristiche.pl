# -*- mode: prolog -*-

:- module(heuristics, [g/2, f/3, manhattan/2, hamming/2, tile_h/2, incorrects/2, correct/2]).

%aggiorna la G
g(G, GNuovo):-
    GNuovo is G + 1.

%calcola la F di un nodo
f(G, Config, FVal):-
    manhattan(Config, H),   
    %hamming(Config, H),  
    FVal is G + H.

% manhattan/2 - manhattan(+Config, -N)
%4X4
manhattan([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,9),tile(2,3,10),tile(3,3,11),tile(4,3,12),tile(1,4,13),tile(2,4,14),tile(3,4,15),tile(4,4,0)], 0).
%3X3
%manhattan([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(1,2,4),tile(2,2,5),tile(3,2,6),tile(1,3,7),tile(2,3,8),tile(3,3,0)], 0).

manhattan(Config, N) :-
    % Recupera tutte le tile in posizione scorretta
    % N è la somma delle loro distanze di manhattan
    incorrects(Config, Incorrects),
    h_aux(Incorrects, N).

h_aux([], 0).
h_aux([TileH|Tiles], Num) :-
    tile_h(TileH, M),
    h_aux(Tiles, N),
    Num is N + M.

% tile_h(+Tile, -N)
tile_h(tile(X, Y, Z), 0) :-
    % Se la tile è in posizione corretta, la sua euristica è 0
    correct(Z, tile(X, Y, Z)), !.

tile_h(tile(X, Y, Z), N) :-
    % Se la tile non è corretta, la sua h è la distanza di manhattan
    % dalla posizione corretta
    correct(Z, tile(I, J, Z)),
    N is abs((J - Y)) + abs((I - X)).
%4X4
hamming([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,9),tile(2,3,10),tile(3,3,11),tile(4,3,12),tile(1,4,13),tile(2,4,14),tile(3,4,15),tile(4,4,0)], 0).
%3X3
%hamming([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(1,2,4),tile(2,2,5),tile(3,2,6),tile(1,3,7),tile(2,3,8),tile(3,3,0)], 0).

hamming(Config, N) :-
    incorrects(Config, Incorrects),
    length(Incorrects, N).

% incorrects/2 - incorrects(+Config, -Tiles)
% Trova le tiles fuori posto, data una configurazione
incorrects([], []).
incorrects([tile(X,Y,Z)|ConfigT], [tile(X, Y, Z)|Tiles]) :-
    correct(Z, tile(I,_,Z)),
    I \== X,
    incorrects(ConfigT, Tiles), !.
incorrects([tile(X,Y,Z)|ConfigT], [tile(X, Y, Z)|Tiles]) :-
    correct(Z, tile(_,J,Z)),
    J \== Y,
    incorrects(ConfigT, Tiles), !.
incorrects([tile(X,Y,Z)|ConfigT], Tiles) :-
    correct(Z, tile(X, Y, Z)),
    incorrects(ConfigT, Tiles).


% correct/2 - correct(+N, -Correct)
% Restituisce le coordinate corrette dato il numero di una tile
correct(N, Correct) :-
    correct_aux(
        N,
        %4X4
        [tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,9),tile(2,3,10),tile(3,3,11),tile(4,3,12),tile(1,4,13),tile(2,4,14),tile(3,4,15),tile(4,4,0)],
        %3X3
        %[tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(1,2,4),tile(2,2,5),tile(3,2,6),tile(1,3,7),tile(2,3,8),tile(3,3,0)],
        Correct
    ).

% correct_aux/3 - correct_aux(+N, +Config, -Correct)
correct_aux(N, [], N) :- !.
correct_aux(N, [tile(X, Y, N)|_], tile(X, Y, N)) :- !.
correct_aux(N, [tile(_, _, Z)|Tiles], Correct) :-
    N \== Z,
    correct_aux(N, Tiles, Correct), !.