:- module(path_search, [
		       solve/3,
		       get_strategy/1,
	               ds_on/0,
	               ds_off/0
		       ]).
:- style_check(-discontiguous).
:- include('../unit_checker').

unit(path_search, []).
%  Ricerca di cammini: i nodi di frontiera memorizzano cammino,
%  costo ed euristica.
%  Usa le strategie messe nella

:- use_module(ga, [solve/3,
		   ds_on/0,
		   ds_off/0,
		   get_size/2,
		   get_nodes/2]).
%  ga � l'algoritmo generico; qui si chiudono i tipi aperti
%  f_node e info, in modo da ottenere i cammini e gestire
%  ricerche limitate
import_type(ga, [p_node/0,
		 fringe/0]).

:- use_module(path_strategy_manager, [get_strategy/1]).
import_type(path_strategy_manager, [
		node_info/0  %informazioni sulle bounded
	    ]).

%==================================================================
%  1.  NODI DI FRONTIERA E SOLUZIONI

type([pn(p_node, list(p_node), number, number)]:f_node).
%   pn(P, Path, Cost, Heur):   nodo di cammino(path node):
%   - P nodo di problema
%   - Path [P|Path] cammino all'inverso
%   - Cost costo del cammino [P|Path]
%   - Heur = euristica h(P)
type([sol(p_node, list(p_node), number)]:solution).
%  sol(G, Path, Cost) :  Path cammino da nodo start
%  a nodo goal G,  con costo Cost

%  2.  Predicati aperti  ================================
%
%	A) Predicati che il problema deve definire
%
open_pred(neighbours(p_node, list(p_node))).
%  lista dei nodi collegati da archi uscenti nel problema
%
open_pred(cost(p_node, p_node, number)).
%  OPZIONALE: cost(N1,N2,C):  (N1,N2) arco con  costo C
%  MODO  (+,+,-) det.  Default 1
open_pred(heuristic(p_node, number)).
%   heuristic(P,H):  H valore euristico di P
%   MODO:  (+,+,-) det
%   OPZIONALE, default 0

%      B)  Predicati aperti legati alle strategie

open_pred(prune(f_node, p_node)).
%  prune(FN, PN) : l'espansione di FN con il vicino PN � potata.
%  DFEFAULT nessuna potatura:
open_pred(store_closed(p_node)).
% store_closed(P) : memorizza P nei nodi chiusi, se la strategia di
% potatura dei chiusi � attiva
% DEFAULT successo senza far niente:
open_pred(update_increment(number)).
%  update_increment(D) implementato da iterative deepening
%  valuta l'incremento minimo per la prossima iterazione
%  DEFAULT successo senza far niente:


%  DEFAULTS PER I PREDICATI APERTI DI PROBLEMA
cost(_,_,1).       %costo 1
heuristic(_,0).    %euristica 0

%  DEFAULTS per pruning e iterative deepening
prune(_,_) :- fail.  % nessun pruning
store_closed(_).     % nessun nodo viene messo fra i chiusi
update_increment(_). % nessun incremento in assenza iterative deepening
start_pruning :-
	retractall(path_search:closed(_)).


%  3.  CHIUSURA predicati aperti di ga

ga:start_node(P,pn(P,[],0, H), BoundStrategy) :-
	start_pruning,
	heuristic(P,H),
	get_strategy(Strategy),!,
	set_f_strategy(Strategy),
	set_p_strategy(Strategy),
	set_b_strategy(Strategy, BoundStrategy).
ga:goal_node(fr(BS,_), Goal, pn(P,Path,Cost,Heur), sol(P,RevPath,Cost)) :-
	check_lower_bounds(BS, pn(P,Path,Cost,Heur)),!,
	call(Goal, P),
	reverse([P|Path], RevPath).

ga:expand_nodes(FR, pn(PN,Path,Cost,Heur), Expanded) :- !,
	neighbours(PN,Vicini),
	extend_paths(FR,pn(PN,Path,Cost,Heur),Vicini,[],Expanded),
	store_closed(PN).

% 4.  PREDICATI LOCALI
%
local_pred(check_lower_bounds(node_info, f_node)).
%   check_bounds(Bounds, FN) :  il livello di FN supera i limiti
%   inferiori sullo spazio di ricerca; per ora solo iterative
%   deepening prevede un limite inferiore
%   MODO  (+,+) semidet
%
local_pred(extend_paths(fringe, f_node, list(p_node), list(f_node), list(f_node))).
%  extend_paths(FR, FN, LV, Exp1,Exp2) :
%  FN � il nodo di frontiera da espandere mediante i vicini non ancora
%  considerati LV; si ha:
%  Exp2 = Exp1 unito alle estensioni di FN non potate
%  MODO (+,+,+,+,-) det.
local_pred(depth(p_node, integer)).
local_pred(cost(p_node, number)).
local_pred(f(p_node, number)).
local_pred(priority(p_node, number)).
%  predicati di livello usabili da una bounded strategy
%  -  depth per limiti sulla profondit�, cost sul costo
%     f sulla f di astar, priority sulla priorit� usata
%     dalla frontiera
%  MODO (+,-) det


check_lower_bounds(no_bound, _) :-!.
check_lower_bounds(id_limit(E,Min,_,_), FN) :-!,
	call(E, FN, L),
	L > Min.
check_lower_bounds(lev_limit(_,_),_FN) :-!.


extend_paths(_,_, [],E, E).
extend_paths(FR, FN, [V|NextV],E1, E2) :-
	prune(FN, V), !,
	%   Taglio dei cicli o dei chiusi
	extend_paths(FR, FN, NextV,E1, E2).
extend_paths(fr(lev_limit(E,Max),Fr), pn(N,Path,Cost, Heur),
	     [V|NextV],Expanded1, Expanded2) :-!,
	cost(N,V,C),
	Cost1 is Cost+C,
	heuristic(V, Heur1),
	call(E, pn(N,Path,Cost,Heur), Level),
	%  Level bounded by Max
	(   Level < Max ->
	    Expanded = [pn(V,[N|Path],Cost1,Heur1)|Expanded1]
	;   Expanded=Expanded1  ),!,
	extend_paths(fr(lev_limit(E,Max),Fr), pn(N,Path,Cost,Heur),
		     NextV, Expanded,Expanded2).
extend_paths(fr(id_limit(E,Min,Max,Up),Fr), pn(N,Path,Cost, Heur),
	     [V|NextV],Expanded1, Expanded2) :-!,
	cost(N,V,C),
	Cost1 is Cost+C,
	heuristic(V, Heur1),
	call(E, pn(N,Path,Cost,Heur), Level),
	%  Level bounded by Max in id
	(   Level =< Max ->
	    Expanded = [pn(V,[N|Path],Cost1,Heur1)|Expanded1]
	;   Increment is Level-Max,
	    update_increment(Increment),
	    Expanded=Expanded1  ),!,
	extend_paths(fr(id_limit(E,Min,Max,Up),Fr), pn(N,Path,Cost,Heur),
		     NextV, Expanded,Expanded2).
extend_paths(fr(BS,Fr), pn(N,Path,Cost, Heur),
	     [V|NextV],Expanded1, Expanded2) :-
	cost(N,V,C),
	Cost1 is Cost+C,
	heuristic(V, Heur1),!,
	extend_paths(fr(BS,Fr), pn(N,Path,Cost,Heur),
		     NextV, [pn(V,[N|Path],Cost1,Heur1)|Expanded1],Expanded2).

depth(pn(_PN,Path,_Cost,_Heur), L) :-
	%  profondit�
	length(Path, L).
cost(pn(_PN,_Path,Cost,_Heur), Cost).
        % costo
f(pn(_PN,_Path,Cost,Heur), F) :-
	%  funzione f di A*
	F is Cost+Heur.
priority(FN, P) :-
	% priorit� nella strategia attualmente in uso
	ga:f_priority(FN, P).


set_f_strategy(Strategy) :-
	member(f_strategy(FS), Strategy), !,
	atom_concat('f_strategy/', FS, SClosure),
	use_strategy(SClosure)
	;
	use_strategy('f_strategy/astar').
set_p_strategy(Strategy) :-
	member(p_strategy(FS), Strategy), !,
	atom_concat('p_strategy/', FS, SClosure),
	use_strategy(SClosure)
	;
	use_strategy('p_strategy/no_cut').
set_b_strategy(Strategy, BS) :-
	member(lev_limit(L, Max), Strategy),!,
	BS = lev_limit(L, Max),
	use_strategy('id_strategy/no_id')
	;
	member(id_limit(L, Min, Max, Up), Strategy),!,
	BS = id_limit(L, Min, Max, Up),
	use_strategy('id_strategy/id'),
	start_id
	;
	BS = no_bound,
	use_strategy('id_strategy/no_id').

use_strategy(S) :-
	module_property(path_search, file(Path)),
	file_directory_name(Path, Dir),
	atomic_list_concat([Dir,'/', S], PS),
	consult(PS).

%=====================================================

debug_and_stats:write_fringe(fr(Info, FR), Show) :-
	get_size(fr(Info, FR), Size),
	maplist(write, ['Frontiera ', fr(Info, '...'), ' con ', Size, ' nodi\n']),
	(   Show = true ->
	    get_nodes(fr(Info, FR), Nodes),
	    writeln('['),
	    maplist(write_problem_node, Nodes),
	    writeln(']')
	;   true).


debug_and_stats:write_node(pn(PN, Path, Cost, Heur)) :-
	write_problem_node(pn(PN, Path, Cost, Heur)),
	write('vuoi il cammino? s_i/ RET : '),
	readln(Risp),
	(   Risp=[s|_]  -> write_path(Path)
	;   true).

debug_and_stats:write_pnode(pn(PN, Path, G, H)) :-
	write_problem_node(pn(PN, Path, G, H)).

write_problem_node(pn(PN, _Path, G, H)) :-
	F is G+H,
	maplist(write, ['   ', PN, ', f:g:h = ', F:G:H,  '\n']).

write_path(P) :-
	writeln('['),
	maplist(writeln, P),
	writeln(']').
