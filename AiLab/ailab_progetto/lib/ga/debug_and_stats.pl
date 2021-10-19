:- module(debug_and_stats, [set_ds_level/1,
			    start_trace/2,
			    das_trace/2,
			    solve_trace/2,
			    search_trace/2]).


:- nb_setval(ds_level, 0).

set_ds_level(L) :-
	nb_setval(ds_level, L).

level_ok(L) :-
	nb_getval(ds_level, K),
	L =< K.



start_trace(K, start(FN, fr(Info, Empty), Start, Goal)) :-
	init_stats,
	tr(K,start_trace(K, start(FN, fr(Info, Empty), Start, Goal))).
das_trace(K, expand_nodes(Fringe, FN, Expansion)) :-!,
	tr(K, expand_nodes(Fringe, FN, Expansion)),
	length(Expansion, N),
	increment_nodes(N).
das_trace(_,_).


solve_trace(K, solve(FN, fr(NewInfo, Empty), Goal, Sol)) :-
	tr(K, solve_trace(K, solve(FN, fr(NewInfo, Empty), Goal, Sol))).
        %['ID: nuova iterazione con nodo start ', FN, ' e incremento ', NewInfo]).

search_trace(K, goal_node(Fringe, Goal, FN, Sol)) :-
	tr(K, search_trace(K, goal_node(Fringe, Goal, FN, Sol))),
	print_stats(FN).
search_trace(K, search(NextFN, NextFringe, Goal, Sol)):-
	tr(K, search_trace(K, search(NextFN, NextFringe, Goal, Sol))).


%=============================  TRACING

tr(L, Msg) :-
	level_ok(L),
        step(L),!,
	write_msg(Msg)
	;
	nb_getval(prompt_number, K),
	H is K+1,
	nb_setval(prompt_number, H).


write_msg(start_trace(_K, start(FN, _, Start, Goal))) :-
	maplist(write,
		['\nInizio con predicati start ', Start, ' e di goal ', Goal, '\n']),
	write('Nodo iniziale: '),
	write_pnode(FN).


write_msg(expand_nodes(_Fringe, FN, Expansion)) :-
	write('NODO ESPANSO --->'),
	write_pnode(FN),
	write('\nEspansione:\n'),
	maplist(write_pnode, Expansion).


write_msg(search_trace(_K, goal_node(_Fringe, _Goal, FN, _Sol))) :-
	writeln('\nRaggiunto un nodo GOAL:'),
        write_node(FN),
	writeln('\nStatistiche:'),
	print_stats(FN).

write_msg(search_trace(_K, search(_NextFN, NextFringe, _Goal, _Sol))) :-
	write('\nNuova frontiera: '),
	nb_getval(show_fringe, Show),
	write_fringe(NextFringe, Show).

write_msg(solve_trace(_K, solve(_FN, fr(NewInfo, _Empty), _Goal, _Sol))) :-
	maplist(write, ['\nID: nuova iterazione con incremento ', NewInfo]).


step(L) :-
	nb_getval(prompt_number, K),
	maplist(write, ['\nStep ', L:K, ' s-kip/a-bort/t-race/n-otrace/l-ivello/+f-rontiera/-f-rontiera/ RET: ']),
	H is K+1,
	nb_setval(prompt_number, H),
	readln(Command),
	(   Command = [s|_] -> set_ds_level(0), fail
	;   Command = [a|_] -> abort
	;   Command = [t|_] -> trace
	;   Command = [n|_] -> notrace
	;   Command = [l|_] -> nuovo_livello
	;   Command = [+,f|_] -> nb_setval(show_fringe, true)
	;   Command = [-,f|_] -> nb_setval(show_fringe, false)
	;   true).

nuovo_livello :-
	nb_getval(ds_level, K),
	maplist(write, ['Livello ', K, '; nuovo livello / RET : ']),
	readln(Ans),
	(   set_level(Ans), !
	;   write('Input errato, devi inserire un intero >= 0;  e-sci o ripeti : '),
	    nuovo_livello).

set_level([]) :-
	writeln('Livello invariato').
set_level([N|_]) :-
	integer(N),
	N >= 0, !,
	set_ds_level(N),
        maplist(write, ['Ok, nuovo livello = ', N, '\n']).



%============================= STATS
init_stats :-
	nb_setval(expanded_nodes, 0),
	nb_setval(prompt_number, 0),
	nb_setval(show_fringe, false),
	nuovo_livello.
increment_nodes(N) :-
	nb_getval(expanded_nodes, N1),
	N2 is N1+N,
	nb_setval(expanded_nodes, N2).

print_stats(pn(G, Path, Cost, _)) :-
	nb_getval(expanded_nodes, N),
	nb_getval(prompt_number, PN),
	length([G|Path], P),
	maplist(write, ['\n\nnodi espansi: ', N, '; iterazioni: ', PN, '; profondita'' soluzione: ', P, '; costo: ',
			Cost, '\n']),
	B is e^(log(N)/P),
	writeln(b=B).

