

:- use_module(worlds).
:- use_module(worlds).


:- consult('agente_ci').
:- consult('raccoglitore_gerarchico').
:- consult('graphics').


:- style_check(-discontiguous).

h0_on :-
	unload_file('raccoglitore_gerarchico'),
	consult('raccoglitore_gerarchico_h0').
	
h0_off :-
	unload_file('raccoglitore_gerarchico_h0'),
	consult('raccoglitore_gerarchico').


test_on :-
	unload_file('graphics'),
	consult('testing').

test_off :-
	unload_file('testing'),
	consult('graphics').	


start(World,Batteria,Capacita) :-
	start(World,Batteria,Capacita,_).
	
	
start(World,Batteria,Capacita,Results) :-
	set_strategy(astar),
	set_strategy(ps(closed)),
	retractall,
	dimentica_tutto,
	load_world(World),
	assertall(World,Batteria,Capacita),
	start_simulation,
	agente(ok,Goal),
	!,
	get_results(Goal,Results),
	end_simulation(Results).	

	
get_results(Goal, [Goal,T,S,R,C,B]) :-
	aggregate_all(sum(S1),content(_,_,sporco(S1)), X),
	aggregate_all(count,content(_,_,sporco(_S2)), Y),
	S is X + Y,
	maplist(call,[tempo,raccolto,n_carica, n_base],[T,R,C,B]).
	
	
retractall :- 
	retractall(current_world(_)),
	retractall(in(_)),
	retractall(ostacolo(_)),	
	retractall(base(_)),
	retractall(cestino(_)),
	retractall(sporco(_,_)),
	retractall(batteria(_)),
	retractall(capacita(_)),
	retractall(batteria_massima(_)),
	retractall(capacita_massima(_)),
	retractall(tempo(_)),
	retractall(raccolto(_)),
	retractall(n_base(_)),
	retractall(n_carica(_)).
	
assertall(World,Batteria,Capacita) :-
	assert(current_world(World)),
	content(World,Base,base),
	assert(base(Base)),
	content(World,Cestino,cestino),	
	assert(cestino(Cestino)),
	(content(World,In,agente) -> assert(in(In)); assert(in(Base))),
	forall(content(World,Pt,sporco(Qty)),assert(sporco(Pt,Qty))),
	assert(batteria(Batteria)),
	assert(capacita(Capacita)),
	assert(batteria_massima(Batteria)),
	assert(capacita_massima(Capacita)),
	assert(tempo(0)),
	assert(raccolto(0)),
	assert(n_base(0)),
	assert(n_carica(0)).	


	
char_world(world0,[
'#####',
'#B9C#',
'#####']).

char_world(world1,[
'#####',
'#B C#',
'#   #',
'#000#',
'#####']).

char_world(world2,[
'######',
'#B  C#',
'###  #',
'#0#  #',
'#0  0#',
'######']).

char_world(world3,[
'#######',
'#B   C#',
'# ### #',
'#     #',
'# ###0#',
'#00  0#',
'#######']).

char_world(world4,[
'########',
'#C    0#',
'# #### #',
'# #00  #',
'#  00# #',
'# #### #',
'#B    0#',
'########']).

char_world(world5,[
'##########',
'#B      C#',
'# ####   #',
'# ####   #',
'#00  #   #',
'###0 #  0#',
'###0    0#',
'##########']).

char_world(world6,[
'############',
'#B         #',
'######00#  #',
'#0000#00#  #',
'#0001#00#  #',
'#0####00## #',
'#1100000#  #',
'#3122#22# C#',
'############']).

char_world(world7,[
'############',
'#B        C#',
'#       ####',
'# ####  #07#',
'#0023#   00#',
'#1002#  #00#',
'#1000# ##0##',
'#1000000045#',
'#2111#00456#',
'############']).
	
char_world(world8,[
'###############',
'#B            #',
'#    #     ####',
'# ####     #07#',
'#0023#      00#',
'#1002#     #00#',
'#1####000###  #',
'#1000000000#  #',
'#2111#00400  C#',
'###############']).
		
char_world(world9,[
'###################',
'#B              00#',
'#              #23#',
'#    #      #######',
'# ####      #00073#',
'#0023#      #00002#',
'#1002#        #####',
'#1####00####      #',
'#1000000000#      #',
'#2111#00400       #',
'#2111## #######  ##',
'#C         #003210#',
'###################']).		
		