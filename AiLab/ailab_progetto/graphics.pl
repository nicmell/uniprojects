 

:- use_module(library(draw_world)).


world_size(30).


start_simulation :-
	world_size(Size),
	world(W, Width, Height),
	draw_world(W,Width,Height,Size),
	batteria_massima(Batteria),
	capacita_massima(Capacita),
	draw_caption(W,[Batteria,Batteria,Capacita,Capacita,0,0]),
	draw_agent_state(W,Size),
	step(['\nAgente aspirapolvere. Batteria: ', Batteria, ', Capacita: ', Capacita,".\nPremere invio per avviare l' agente..."]).


end_simulation([Goal,T,S,R,C,B]) :-
	(Goal == success ->
		writeln('SUCCESSO!'); writeln('FAIL!')),
	maplist(write,["C'erano ", S, " unita' di sporco nell' ambiente, l' agente ne ha raccolte ", R," in ", T," unita' di tempo.\n"]),
	maplist(write,["L'agente ha ricaricato ", C," unita' di batteria, dovendo andare alla base ", B, " volte.\n"]),
	writeln('Chiudo...'),
	world(W, _Width, _Height),
	destroy(caption(W)),
	destroy(W). 

	
%%%%%%%%%%%%%%%%%%%%%%	
	
update_caption(World) :-
	batteria(Batteria),
	batteria_massima(BatteriaMassima),
	capacita(Capacita),
	capacita_massima(CapacitaMassima),
	raccolto(Raccolto),
	tempo(Tempo),
	draw_world:picture(Caption,caption(World),_,_,_),
	send(Caption,clear),
	draw_caption(World,[Batteria,BatteriaMassima,Capacita,CapacitaMassima,Raccolto,Tempo]).

draw_caption(World,[Batteria,BatteriaMassima,Capacita,CapacitaMassima,Raccolto,Tempo]) :-
	draw_world:picture(Caption,caption(World),_,_,_),
	atom_length(BatteriaMassima,B),
	atom_length(CapacitaMassima,C),
	atomic_list_concat(['Batteria: ~',B,'w/~',B,'w; Capacita: ~',C,'w/~',C,'w; Raccolto: ~4w; Tempo: ~4w'],Fmt),
	format(atom(T),Fmt,[Batteria,BatteriaMassima,Capacita,CapacitaMassima,Raccolto,Tempo]),
	new(Text,text(T, left, font(screen, bold, 14))),
	send(Caption, display(Text, point(0,0))),
	send(Caption, displayed,on),
	send(Caption,redraw).	
	
draw_world(W,Width,Height,Size) :-
	new_picture(W,Width,Height,Size),
	new_picture(caption(W), 34, 1, 18),
	forall(between(1,Height,Row),
		(forall(between(1,Width,Col),draw_content(W,point(Row,Col),Size))
	)).

draw_content(W, P, Size) :-
	forall((content(W,P,C),fig_of(W,C,Fig,Size)), draw_fig(W,Fig,P)).
	
fig_of(_,ostacolo,box(Size,[col(black)]),Size).
fig_of(_,agente,circ(CircSize,[col(green)]), S) :-
	CircSize is truncate(3*S/4).
fig_of(_,goal,circ(CircSize,[col(yellow)]), S) :-
	CircSize is truncate(3*S/4).	
	
draw_agent_state(World,Size) :-
	base(Base),
	draw_fig(World, box(Size,[col(yellow)]), Base),
	cestino(Cestino),
    draw_fig(World, box(Size,[col(green)]), Cestino),	
	in(In),
	draw_fig(World, circ(Size,[col(cyan)]), In),
	forall(sporco(P,_Qty), draw_fig(World, box(Size,[col(blue)]), P)).	
	
	
%%%%%%%%%%%%%%%%%%%%%%	

simula(va(P1,P2)) :- 
	current_world(W),
	move_fig(W,circ(_,_),P1,P2),
	update_caption(W),
	step([]).
	
simula(pulisci(P)) :-
	current_world(W),
	del_fig(W, box(_,_), P),
	update_caption(W),
	step(['raccolgo lo sporco in ', P]).

simula(pulisci_sporco_ostinato(P)) :-
	current_world(W),
	world_size(Size),
	del_fig(W, box(_,_), P),
	del_fig(W, circ(_,_), P),
	draw_fig(World, box(Size,[col(red)]), P),
	draw_fig(World, circ(Size,[col(cyan)]), P),		
	update_caption(W),
	step(['raccolgo lo sporco in ', P]).	
	
simula(svuota) :-
	current_world(W),
	update_caption(W),
	step(['Deposito lo sporco nel cestino']).
	
simula(carica) :-
	step(['Carico la batteria ed elaboro le prossime mosse']),
	current_world(W),
	update_caption(W).

	
simula(chiude) :-
	step(['Chiudo']).

	
%%%%%%%%%%%%%%%%%%%%%%	
	
step(Msg) :-
	maplist(write, Msg),
	nl,
	readln(L),
	(   L=[t|_] -> trace
	;   L=[a|_] -> abort
	;   L=[n|_] -> notrace
	;   true).		