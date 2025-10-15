# -*- mode: prolog -*-

:- module(search, [prova/1, itdeep/2, ric_prof/5, iniziale/1, finale/1]).
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
iniziale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,10),tile(2,3,11),tile(3,3,12),tile(4,3,15),tile(1,4,0),tile(2,4,9),tile(3,4,13),tile(4,4,14)]).
%iniziale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,6),tile(2,2,0),tile(3,2,7),tile(4,2,8),tile(1,3,5),tile(2,3,11),tile(3,3,12),tile(4,3,15),tile(1,4,10),tile(2,4,9),tile(3,4,13),tile(4,4,14)]).

%lo stato finale(goal)
finale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(4,1,4),tile(1,2,5),tile(2,2,6),tile(3,2,7),tile(4,2,8),tile(1,3,9),tile(2,3,10),tile(3,3,11),tile(4,3,12),tile(1,4,13),tile(2,4,14),tile(3,4,15),tile(4,4,0)]).


%--------------versione 3X3------------
% Easy
%iniziale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(1,2,0),tile(2,2,6),tile(3,2,8),tile(1,3,4),tile(2,3,7),tile(3,3,5)]).

% Medium
%iniziale([tile(1,1,4),tile(2,1,0),tile(3,1,5),tile(1,2,1),tile(2,2,8),tile(3,2,2),tile(1,3,6),tile(2,3,7),tile(3,3,3)]).

%finale([tile(1,1,1),tile(2,1,2),tile(3,1,3),tile(1,2,4),tile(2,2,5),tile(3,2,6),tile(1,3,7),tile(2,3,8),tile(3,3,0)]).


prova(Soluzione):-
    time(itdeep(1, Soluzione)),
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

%itdeep(+Limite, -ListaAzioni)
% Algoritmo non ha ancora trovato la soluzione
itdeep(Limite, Soluzione):-
    iniziale(S),
    regole: azioni(S, [S], ListaAzioni),
    ric_prof(nodo(S,0), [S], ListaAzioni, Soluzione, Limite),
    NuovoLimite is Limite + 1,
    itdeep(NuovoLimite, Soluzione).     
    
% Algoritmo ha trovato la soluzione:


%Stato finale raggiunto
ric_prof(nodo(S, _), _, _, _,_):- finale(S), write('Raggiunto stato finale').

%Stato finale non ancora raggiunto, limite non raggiunto
ric_prof(nodo(S, G), Visitati, [Azione|_], [Azione|Soluzione], Limite):-
    regole:trasforma(Azione, S, SNuovo),
    write('Stato: '), writeln(SNuovo),
    heuristics: g(G, GNuovo),
    write('GNuovo: '), writeln(GNuovo),
    %trace,
    GNuovo < Limite,
    regole: azioni(S, Visitati, AzioniNuove),
    writeln(AzioniNuove),
    ric_prof(nodo(SNuovo, GNuovo), [SNuovo|Visitati], AzioniNuove, Soluzione, Limite).

% limite raggiunto
ric_prof(nodo(S, G), Visitati, [Azione|AzioniTail], [Azione|Soluzione], Limite):-
    regole:trasforma(Azione, S, SNuovo),
    write('Stato: '), writeln(SNuovo),
    heuristics: g(G, GNuovo),
    write('GNuovo: '),writeln(GNuovo),
    GNuovo >= Limite,
    ric_prof(nodo(S, G), [SNuovo|Visitati], AzioniTail, Soluzione, Limite).

ric_prof(_, _, [], [], _).