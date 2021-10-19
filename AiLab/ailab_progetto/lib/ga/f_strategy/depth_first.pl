

ga:f_empty(I,fr(I,[])).

ga:f_select(fr(I,[NF|F]), NF, fr(I,F)).

ga:f_add(fr(I, F1), [N|L],fr(I, [N|F2])) :-
	ga:f_add(fr(I,F1),L, fr(I,F2)).
ga:f_add(Frontiera, [], Frontiera).

ga:f_priority(pn(N,Path,_,_), P) :-
	length([N|Path],P).

ga:f_size(fr(_,L), N) :-
	length(L,N).

ga:fringe_to_list(fr(_I, List), List).
