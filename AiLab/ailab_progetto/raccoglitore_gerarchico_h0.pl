
:- use_module(two_level_planner).
:- use_module(worlds).

:- style_check(-discontiguous).

%===============================================================
%  Agente "Raccoglitore".  Un esempio di implementazione di un agente
%  pianificatore gerarchico su una gerarchia di 2 livelli
%==============================================================

type([in(point), deve_prendere(punto), cestino(punto), fine]:fluent).
%   in(P):  l'agente si trova in P
%   deve_prendere(X): l'agente deve prendere un oggetto in X
%   cestino(Q):  deve depositare gli oggetti in Q
%   fine:  ha finito

type([va(point,point), va_da_a(point,point), raccoglie, deposita]:azione).
%  va(P1,P2) : avanza da P1 a P2 adiacente
%  va_da_a(P1,P2) : va da P1 a P2 non adiacente, macro-azione
%  raccoglie:  raccoglie l'oggetto
%  deposita: deposita gli oggetti

%================================================
%  Predicati dinamici che rappresentano lo stato di
%  mondo+agente
%=================================================
%
pred(in(point)).
%  in(P) :  l'agente si trova in P
:- dynamic(deve_prendere/1).

pred(deve_prendere(point)).
%  deve_prendere(P) :  deve prendere un oggetto in P
:- dynamic(deve_prendere/1).

pred(cestino(point)).
%  cestino(P) :  l'agente deve depositare in P
:- dynamic(cestino/1).

pred(current_world(any)).
%  current_world(W):  W è il mondo in cui si trova l'agente
:- dynamic(current_world/1).

%==================================================
%  Implemento i predicati aperti di two_level_planner
%===================================================

%=====================================
%    AZIONI DI LIVELLO 0
%=====================================

	
% VA AL PROSSIMO OGGETTO
%l'aspirapolvere ha possibilità di raccogliere altri oggetti	
two_level_planner:add_del(0, va_a_pulire(P2), St, [in(P2),batteria(B2),capacita(C2), raccolto(R2)|Sporco], [in(P1),batteria(B1),capacita(C1), raccolto(R1), sporco(P2,Qty1)], Cost1) :-
	member(capacita(C1),St),
	C1 > 0,
	member(in(P1), St),
	member(sporco(P2,Qty1),St),
	get_action_plan([in(P1),sporco(P2)], va_a_pulire(P2), _Plan, Cost1),
%	foreach(member(va(_,P4),Plan), (P2 == P4; not(member(sporco(P4),St)))),
	member(batteria(B1),St),
	B2 is B1 - Cost1,
	B2 >= 0,
	(
	sa(P2,sporco_ostinato) -> 
		(Qty1 == 0 -> 
			(Sporco = []);
			(Qty2 is Qty1 - 1, Sporco = [sporco(P2,Qty2)]) 
		)
		; (Sporco = [])
	),
	member(capacita(C1),St),
	C2 is C1 - 1, 
%	(sa(P1,sporco_ostinato) -> Fine = [fine]; Fine = []),
	member(raccolto(R1),St),
	R2 is R1 + 1.


% DEPOSITA SPORCO
%l'aspirapolvere si trova sul cestino e ha dello sporco da depositare
two_level_planner:add_del(0, va_a_svuotare, St, [in(P2),batteria(B2),capacita(C2)], [in(P1),batteria(B1),capacita(C1)], Cost) :-
	% la capacita deve essere diversa dalla capacita massima
	member(capacita(C1),St),
	capacita_massima(C2),
	C1 \= C2,
	% parto da P1 e vado al cestino in P2
	member(in(P1), St),
	member(cestino(P2),St),
	% RULE 1: la capacià è a zero, altrimenti non deve esserci sporco tra P1 e P2
	%(C1 == 0; foreach(member(sporco(P),St), not(point_between(P,P1,P2)))),
	%
	get_action_plan([in(P1),cestino(P2)], va_a_svuotare, _Plan, Cost),
	member(batteria(B1),St),
	B2 is B1 - Cost,
	B2 >= 0,
	member(capacita(C1),St),
	capacita_massima(C2).

% CARICA
%l'aspirapolvere si trova su dello sporco e lo raccoglie	
two_level_planner:add_del(0, va_a_caricare, St, [in(P2),batteria(B3)|Fine], [in(P1),batteria(B1)], Cost) :-
	% la batteria deve essere diversa dalla batteria massima
	member(batteria(B1),St),
	batteria_massima(B3),
	B1 \= B3,
	% parto da P1 e vado alla base in P2
	member(in(P1), St),
	member(base(P2),St),
	% RULE 2: se la capacità è diversa dalla capacita massima, non vi deve essere il cestino fra P1 e P2
	%member(cestino(Cestino),St),
	%member(capacita(C1),St),
	%capacita_massima(C3),
	%(C1 == C3; not(point_between(Cestino,P1,P2))),
	% RULE 1: la capacià è a zero, altrimenti non deve esserci sporco tra P1 e P2
	%(C1 == 0; foreach(member(sporco(P),St), not(point_between(P,P1,P2)))),
	%
	get_action_plan([in(P1),base(P2)], va_a_caricare, _Plan, Cost),
	member(batteria(B1),St),
	B2 is B1 - Cost,
	B2 >= 0,
	batteria_massima(B3),
	((
		findall(S,member(sporco(S,_),St),L),
		L == [],	
		member(capacita(C),St),
		capacita_massima(C)
	) -> 
	Fine = [fine]; 
	Fine=[]
	).
	

	

%=====================================
%    EURISTICA DI LIVELLO 0
%=====================================


two_level_planner:h(0, _St, 0).

%=======================================
%   PREDICATI DI START E  GOAL LIVELLO 0
%=======================================

two_level_planner:starting_state(_,
	[in(In), base(Base), cestino(Cestino), batteria(Batteria), capacita(Capacita), raccolto(0) | Sporco]) :-
	findall(sporco(P,9), sporco(P,_Qty), Sporco),
	in(In),
	base(Base),
	cestino(Cestino),
	batteria(Batteria),
	capacita(Capacita).	

two_level_planner:goal_state([State,State], Goal) :-
	member(fine, Goal).

%=====================================
%    AZIONI DI LIVELLO 1
%=====================================

two_level_planner:add_del(1, va(P1,P2), St, [in(P2)], [in(P1)], 1) :-
	member(in(P1), St),
	step(_,P1,P2),
	current_world(W),
	not(content(W,P2,ostacolo)).

two_level_planner:add_del(1, pulisci(P), St, [fine], [], 1) :-
	member(in(P), St),
	member(sporco(P), St).

two_level_planner:add_del(1, svuota, St, [fine], [], 1) :-
	member(in(P), St),
	member(cestino(P), St).

two_level_planner:add_del(1, carica, St, [fine], [], 0) :-
	member(in(P), St),
	member(base(P), St).	
		
%=====================================
%    EURISTICA DI LIVELLO 1
%=====================================
two_level_planner:h(1, St, H) :-
	(member(sporco(P2), St); member(cestino(P2), St); member(base(P2), St)),
	member(in(P1),St),
	distance(manhattan, P1,P2,H).

%=======================================
%   PREDICATI DI START E  GOAL LIVELLO 1
%=======================================

two_level_planner:action_starting_state(State, _Action, State).
	
two_level_planner:action_final_state(_State, _Action, Goal) :-
	member(fine,Goal).


