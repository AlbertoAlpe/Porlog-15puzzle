# -*- mode: prolog -*-

:- module(search, [prova/1, idastar/2, dfs_aux/6, list_min/2, iniziale/1, finale/1]).
:- use_module('./regole.pl').
:- use_module('./euristiche.pl').

%per modificare la dimensione dello stack
%:- set_prolog_stack(global, limit(100 000 000 000)).

% Benchmarks

% Hard test:
%iniziale([tile(1,1,11),tile(2,1,7),tile(3,1,1),tile(4, 1,5),tile(1,2,3),tile(2,2,9),tile(3,2,4),tile(4,2,8),tile(1,3,2),tile(2,3,13),tile(3,3,12),tile(4,3,15),tile(1,4,6),tile(2,4,14),tile(3,4,10),tile(4,4,0)]).

% Medium tests:
%iniziale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,15),tile(2,3,0),tile(3,3,14),tile(4,3,11),tile(1,4,10),tile(2,4,12),tile(3,4,9),tile(4,4,13)]).

% Easy test:
%iniziale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,10),tile(2,3,11),tile(3,3,12),tile(4,3,15),tile(1,4,0),tile(2,4,9),tile(3,4,13),tile(4,4,14)]).
iniziale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,6),tile(2,2,0),tile(3,2,7),tile(4,2,8),tile(1,3,5),tile(2,3,11),tile(3,3,12),tile(4,3,15),tile(1,4,10),tile(2,4,9),tile(3,4,13),tile(4,4,14)]).

%lo stato finale(goal)
finale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,9),tile(2,3,10),tile(3,3,11),tile(4,3,12),tile(1,4,13),tile(2,4,14),tile(3,4,15),tile(4,4,0)]).


%--------------versione 3X3------------
% Easy
%iniziale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(1,2,0),tile(2,2,6),tile(3,2,8),tile(1,3,4),tile(2,3,7),tile(3,3,5)]).

% Medium
%iniziale([tile(1,1,4),tile(2,1,0),tile(3,1,5),tile(1,2,1),tile(2,2,8),tile(3,2,2),tile(1,3,6),tile(2,3,7),tile(3,3,3)]).

%finale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(1,2,4),tile(2,2,5),tile(3,2,6),tile(1,3,7),tile(2,3,8),tile(3,3,0)]).


prova(Soluzione):-
    iniziale(S),
    heuristics: f(0, S, F), 
    time(idastar(F, Soluzione)),
    write('Lista di azioni: '),
    write_actions(Soluzione).

%write_actions/1 stampa le azioni che permettono di arrivare alla soluzione, se trovata  
write_actions([]).

write_actions([Action]):-
    write(Action),
    writeln('.').

write_actions([Action|Rest]):-
    write(Action),
    write(' | '),
    write_actions(Rest).

%idastar(+Soglia, -ListaAzioni)
% Algoritmo non ha ancora trovato la soluzione
idastar(Soglia, Soluzione):-
    iniziale(S),
    heuristics: f(0, S, F),
    regole: azioni(S, [S], ListaAzioni),
    write('Soglia: '),
    writeln(Soglia),
    findall(X, dfs_aux(nodo(F,0,S), ListaAzioni, Soluzione, [S], Soglia, X), Fexceeded),
    write('Fexceeded: '),
    writeln(Fexceeded),
    \+member(-1, Fexceeded),
    !,
    list_min(Fexceeded, NuovaSoglia),
    idastar(NuovaSoglia, Soluzione).     
    
% Algoritmo ha trovato la soluzione:
% esegue idastar un'ultima volta, con il valore di soglia sufficiente a raggiungere finale(S) 
idastar(Soglia, Soluzione):- 
    writeln('Iterazione finale'),
    iniziale(S),
    heuristics: f(0, S, F),
    regole: azioni(S, [S], ListaAzioni),
    dfs_aux(nodo(F,0,S), ListaAzioni, Soluzione, [S], Soglia, -1),!.

% Raggiunto lo stato finale
dfs_aux(nodo(_,_,S),[],_,_,_,-1):- 
    finale(S),
    write('Stato finale raggiunto'),
    !, 
    trace.

% Ricerca non eccede la soglia: si prosegue in profondità
dfs_aux(nodo(F,G,S), [Azione|_], [Azione|Soluzione], Visitati, Soglia, NuovaSoglia):-
    F =< Soglia, 
    regole:trasforma(Azione, S, SNuovo),
    heuristics: g(G, GNuovo),
    heuristics: f(GNuovo, SNuovo, FNuovo),
    regole: azioni(SNuovo, Visitati, AzioniNuove),
    dfs_aux(nodo(FNuovo, GNuovo, SNuovo), AzioniNuove, Soluzione, [SNuovo|Visitati], Soglia, NuovaSoglia).


% Ricerca eccede la Soglia: rieseguo ricerca con soglia maggiore
dfs_aux(nodo(F,_,_),_,_,_,Soglia,F):- F>Soglia.  


%trova il valore minimo in una lista (usato per F)
list_min([L|Ls], Min) :-
    list_min(Ls, L, Min).

list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).