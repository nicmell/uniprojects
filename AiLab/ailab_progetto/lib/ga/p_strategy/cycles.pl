

%  Chiude i predicati di pruning per la strategia di taglio dei
%  cicli su cammino singolo.

path_search:start_pruning.

path_search:prune(pn(N, Path, _,_), V) :-
	member(V, [N|Path]).

path_search:store_closed(_,_).

