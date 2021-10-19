

:- use_module(worlds).

:- style_check(-discontiguous).

	
agente(Stato, Obiettivo) :-
	fine(Stato, Obiettivo),!
	;
	decidi(Stato, Obiettivo, Decisione),
	pianifica(Stato1, Decisione, Piano),!,
	%  esecuzione passo passo, aiuta nel debugging
	step(['Decisione: ', Decisione, '\nPiano: ', Piano]),
	catch(esegui([],Piano, Stato1, NuovoStato),
	       failed(EnvA, Causa, PianoA),
	       % in caso di eccezione di fallimento passo
	       % in stato di fallimento
	       NuovoStato = failed(EnvA, Causa, PianoA)),
	agente(NuovoStato, Obiettivo).	


fine(fine,success) :-
	not(sporco(_,_)),
	base(B),
	in(B),
	capacita(C),
	capacita_massima(C).
	
fine(fine,fail).

decidi(failed(_Env, pulisci(_P),_Piano),_,piano(ok)).
decidi(failed(_Env, va(_P1,_P2),_Piano),_,piano(ko)).
decidi(State,_,piano(State)).	
decidi(_,_,esecuzione(chiudi)).


pianifica(_Stato, esecuzione(A), [A]).
pianifica(Stato,  piano(Decisione), Piano) :-
	time(get_plan([Stato, Decisione], Piano, _Cost)).

	
esegui(_Env, [], Stato, Stato).
%  Piano terminato con successo
esegui(Env,[A|Piano], Stato1, Stato2) :-
	% esecuzione azione e prosecuzione
	catch( esegui_azione(Env, A, Stato1, Stato),
	       failed(EnvA, Causa),
	       % in caso di eccezione di fallimento, la rilancio
	       % includendo l'informazione sul Piano ancora da eseguire
	       throw(failed(EnvA, Causa, Piano))),
	esegui(Env,Piano, Stato, Stato2).

esegui_azione(Env, A, Stato1, Stato2) :-
	macro_azione(A),!,
	action_starting_state(A, St0),
	% lo strips_state St0 rappresenta lo stato attuale del mondo,
	% .. per calcolare il piano per A a partire dallo stato attuale
	get_action_plan(St0,A,PianoAzione,_),!,
	esegui([A|Env], PianoAzione, Stato1, Stato2).
esegui_azione(Env, A, Stato1, Stato2) :-
	esegui_azione_base(Env, A, Stato1, Stato2),
	(   Stato2=failed(Causa) ->
	    % lo stato di fallimento interrompe il processo di esecuzione
	    throw(failed([A|Env], Causa))
	;   true).	

	
macro_azione(va_a_pulire(_)).
macro_azione(va_a_svuotare).
macro_azione(va_a_caricare).
	
action_starting_state(va_a_pulire(P2),[in(P1), sporco(P2)]) :-
	in(P1),
	(sporco(P2,_Qty);sa(P2,sporco_ostinato)).

action_starting_state(va_a_svuotare,[in(P1), cestino(P2)]) :-
	in(P1),
	cestino(P2).	

action_starting_state(va_a_caricare,[in(P1), base(P2)]) :-
	in(P1),
	base(P2).		
	

esegui_azione_base(_Env,chiudi,ok,fine).


esegui_azione_base(_Env,va(P1,P2),ok,failed(va(P1,P2))) :-
	in(P1),
	sporco(P1,_),
	step(['Trovato sporco imprevisto lungo il cammino. Ricalcolo del piano...\n']). 	
	
esegui_azione_base(_Env,va(P1,P2),State,ok) :-
	retract(in(P1)),
	assert(in(P2)),
	decrement(batteria),
	increment(tempo),
	simula(va(P1,P2)). 	
	
esegui_azione_base(_Env,pulisci(P),_State,failed(pulisci(P))) :-
	not(sporco(P,_)),
	step(['Pulito lo sporco in ', P, '. Ricalcolo del piano...\n']). 		
	
esegui_azione_base(_Env,pulisci(P),State,ok) :- 
	in(P),
	sporco(P,Qty1),
	decrement(batteria),
	decrement(capacita),
	increment(tempo),
	increment(raccolto),
	retract(sporco(P,Qty1)),
	Qty2 is Qty1 - 1,
	(Qty2 >= 0 -> (
		assert(sporco(P,Qty2)),
		(sa(P,sporco_ostinato); impara(P,sporco_ostinato)),
		simula(pulisci_sporco_ostinato(P))
	); (
		simula(pulisci(P)))
	).	

esegui_azione_base(_Env,svuota,_State,ok) :-
	decrement(batteria),
	capacita_massima(C),
	retractall(capacita(_)),
	assert(capacita(C)),
	simula(svuota).
	
esegui_azione_base(_Emv,carica,_State,ok) :-
	increment(n_base),
	n_carica(X),
	batteria(B1),
	batteria_massima(B2),
	Z is X + B2 - B1,
	retractall(n_carica(_)),
	assert(n_carica(Z)),
	retractall(batteria(_)),
	assert(batteria(B2)),
	simula(carica).


increment(Func) :-
	call(Func,Value1),
	Value2 is Value1 + 1,
	Pred1 =.. [Func,Value1],
	retract(Pred1),
	Pred2 =.. [Func,Value2],
	assert(Pred2).

decrement(Func) :-
	call(Func,Value1),
	Value2 is Value1 - 1,
	Pred1 =.. [Func,Value1],
	retract(Pred1),
	Pred2 =.. [Func,Value2],
	assert(Pred2).		
	
impara(P, Oss) :-
	not(sa(P,Oss)),
	assert(sa(P,Oss)).

dimentica_tutto :- 
	retractall(sa(_,_)).
