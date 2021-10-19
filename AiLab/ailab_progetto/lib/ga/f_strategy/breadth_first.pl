

ga:f_empty(Info, fr(Info,dl(U, U))) :-
	var(U).

ga:f_select(fr(_, dl(F1,_F2,_)),_) :-
	var(F1),
	!,
	fail.
ga:f_select(fr(Info, dl([NF|F1], F2)), NF, fr(Info, dl(F1,F2))).

ga:f_add(fr(Info, dl(F1,[N|U])), [N|L], fr(Info, DL)) :-
	ga:f_add(fr(Info, dl(F1,U)), L,  fr(Info, DL)).
ga:f_add(F, [], F).

ga:f_priority(pn(N,Path,_,_), P) :-
	length([N|Path],P).

ga:f_size(fr(_, dl(F1,F2)), N) :-
	dl_size(dl(F1,F2), N).

ga:fringe_to_list(fr(_I,dl(F1,F2)), List) :-
	dl_to_list(dl(F1,F2), List).

dl_size(dl(F1,F2), 0) :-
	(   var(F1); F1==F2), !.
dl_size(dl([_X|F1], F2), N) :-
	dl_size(dl(F1,F2), M),
	N is M+1.

dl_to_list(dl(F1,F2), []) :-
	(   var(F1); F1==F2), !.
dl_to_list(dl([X|F1], F2), [X|L]) :-
	dl_to_list(dl(F1,F2), L).
