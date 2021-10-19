
:- module(worlds, [
		  load_world/1,
		  world/3,
		  content/3,
		  distance/4,
		  navigable/2,
		  point_of/2,
		  point_between/3,
		  step/3
		 ]).
		 
:- dynamic(world/3).
:-  dynamic(content/3).

% CARICAMENTO MONDI		 
		 
load_world(Name) :-
	char_world(Name, [R|Rows]),
	atom_length(R, Width),
	length([R|Rows],Heigth),
	retractall(content(_,_,_)),
	retractall(world(_,_,_)),
	load_rows(Name, 1, [R|Rows]),!,
	assert(world(Name, Width, Heigth)).
	
load_rows(K, RN, [R|Rows]) :-
	atom_chars(R, LR),
	load_row(K, RN, 1, LR),
	RN1 is RN+1,
	load_rows(K,RN1, Rows).
load_rows(_,_,[]).

load_row(K, RN, CN, [Ch|Chars]) :-
	(   represents(K,Ch, Cnt) ->
	    assert(content(K, point(RN,CN), Cnt))
	;   true),
	CN1 is CN+1,
	load_row(K, RN, CN1, Chars).
load_row(_K, _RN, _CN, []).

represents(_,'#', ostacolo).
represents(_,'A', agente).
represents(_,'B', base).
represents(_,'C', cestino).
represents(_, K, sporco(NK)):-
	member(K,['0','1','2','3','4','5','6','7','8','9']),
	atom_number(K,NK). 

% GEOMETRIA

step(nord, point(X,Y1), point(X,Y2)) :-
    Y2 is Y1 + 1. 
step(est, point(X1,Y), point(X2,Y)) :-
    X2 is X1 + 1.
step(sud, point(X,Y1), point(X,Y2)) :-
    Y2 is Y1 - 1.
step(ovest, point(X1,Y), point(X2,Y)) :-
    X2 is X1 - 1.
	
distance(manhattan, point(X1,Y1), point(X2,Y2), D) :-
	D is abs(X1-X2) + abs(Y1-Y2).
		
navigable(W, P) :-
	point_of(W, P),
	not(content(W, P, ostacolo)).

point_between(point(X,Y),point(X1,Y1),point(X2,Y2)) :-	
	X1 =< X2, Y1 =< Y2,!,
	between(X1,X2,X),between(Y1,Y2,Y).
point_between(point(X,Y),point(X1,Y1),point(X2,Y2)) :-	
	X1 >= X2, Y1 =< Y2,!,
	between(X2,X1,X),between(Y1,Y2,Y).	
point_between(point(X,Y),point(X1,Y1),point(X2,Y2)) :-
	X1 =< X2, Y1 >= Y2,!,
	between(X1,X2,X),between(Y2,Y1,Y).
point_between(point(X,Y),point(X1,Y1),point(X2,Y2)) :-
	X1 >= X2, Y1 >= Y2,!,
	between(X2,X1,X),between(Y2,Y1,Y).
	
point_of(W, point(X,Y)) :-
	world(W, Ncol, Nrow),
	between(1,Nrow,X),
	between(1,Ncol,Y).	
	

