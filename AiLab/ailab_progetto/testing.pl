

:- unload_file('graphics').

test_case(1,world1,10,3).
test_case(2,world1,10,10).
test_case(3,world1,100,3).
test_case(4,world1,100,10).

test_case(5,world2,20,3).
test_case(6,world2,20,10).
test_case(7,world2,100,3).
test_case(8,world2,100,10).

test_case(9,world3,20,3).
test_case(10,world3,20,10).
test_case(11,world3,100,3).
test_case(12,world3,100,10).

test_case(13,world4,20,3).
test_case(14,world4,20,10).
test_case(15,world4,100,3).
test_case(16,world4,100,10).

start_simulation.

end_simulation([Goal,T,S,R,C,B]) :-
	(Goal == success ->
		writeln('SUCCESSO!'); writeln('FAIL!')),
	maplist(write,["C'erano ", S, " unita' di sporco nell' ambiente, l' agente ne ha raccolte ", R," in ", T," unita' di tempo.\n"]),
	maplist(write,["L'agente ha ricaricato ", C," unita' di batteria, dovendo andare alla base ", B, " volte.\n"]),
	writeln('Chiudo...'). 

simula(va(_P1,_P2)).
	
simula(pulisci(P)) :-
	step(['raccolgo lo sporco in ', P]).

simula(pulisci_sporco_ostinato(P)) :-
	step(['raccolgo lo sporco in ', P]).	
	
simula(svuota) :-
	step(['Deposito lo sporco nel cestino']).
	
simula(carica) :-
	step(['Carico la batteria ed elaboro le prossime mosse']).

	
simula(chiude) :-
	step(['Chiudo']).


step(Msg) :-
	append(Msg,['\n'],Msg1),
	maplist(write,Msg1).


exec_mode(normal,h0_off).
exec_mode(h0,h0_on).	
	
test_all :-
	write("\n\n"),
	forall(test_case(N,_,_,_),
		forall(exec_mode(Mode,_Cmd),run_test(N,Mode))),
	write("\n\n"),
	writef(
		"%5c|%12c||%12c|%12c|%12c||%12c|%12c|%12c|\n",
		[n,mode,world,batteria,carica,goal,tempo,raccolto]),	
	forall(between(1,101,_),write('-')),
	write("\n"),	
	write_stats
	;
	write("\n"),
	true.
	
run_test(TestNum,Mode) :-
	maplist(write,['##### TEST CASE ', TestNum,': EXECUTON MODE:', Mode, '\n']),
	retractall(benchmark(TestNum,Mode,_)),
	exec_mode(Mode,Cmd), 
	call(Cmd),
	call(test_case,TestNum,World,Battery,Capacity),
	start(World,Battery,Capacity,Results),
	assert(benchmark(TestNum,Mode,Results)).

write_stats :-
	benchmark(TestNum,Mode,[Goal,T,_S,R,_X,_Y]),	
	test_case(TestNum,W,B,C),
	writef(
		"%5c|%12c||%12c|%12c|%12c||%12c|%12c|%12c|\n",
		[TestNum,Mode,W,B,C,Goal,T,R]),
	fail.
	
