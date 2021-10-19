:- module(tc, [check_clauses/1,
	       checked/2,
	       wrong_clause/1,
	       multiple_context_clause/1,
	       open_context_clause/2
	      ]).
unit(tc,[]).
:- include('../unit_checker').
% :- discontiguous([type/1, pred/1, import_type/2, import_pred/2,
% local_pred/1,open_pred/1]).


import_type(units,[symbol/0,
                   decl_symbol/0,
		   input_decl/0,
		   input_symbol/0,
		   import_directive/0,
		   prototype/0,
		   pattern/0,
		   generator/0,
		   type_def/0,
		   gen_pattern/0,
		   pred_pattern/0,
		   type_decl/0,
		   pred_decl/0,
		   decl_attribute/0,
		   module/0,
		   is_type/0,
		   typed_var/0,
		    atomic_wff/0,
		    body/0]).


:- use_module(unit_error_manager,[
	      assert_unit_error/2,
	      assert_unit_warning/2
	      ]).
import_type(unit_error_manager, [error_context/0,
				 unit_warning/0,
				 unit_error/0
				]).

:- use_module(util, [
		  context_var/2
	 ]).
import_type(util,[]).



:- use_module(loader, [
		 unit_clause/4,
		 sub_type_generator/3,
		 type_gen/3,
		 pred_gen/2
	      ]).
import_type(loader,[]).



type([dl(list(typed_var), list(typed_var))]:context).

type([integer:checked_clause]:numbered_checked_clause).

type([clause(list(list(typed_var)), atomic_wff, body),
      wrong_clause(atomic_wff, body)]:checked_clause).




%==============================================================
%    TERM TYPE CHECKING

local_pred(chk_type(atom, context, any, is_type)).
chk_type(Unit, Context, Var, Type) :-
	var(Var), !,
	insert_context(Var:Type, Context, Unit).
chk_type(Unit, Context, Term, Type) :-
	(
	type_gen(call(U, Pred), Type, Unit),
	%not(var(Term)), %ground(Term),
	catch((U:call(Pred, Term), Context=dl(X,X)),_,fail)
	;
	type_gen(gen(Term, Args, Types), Type, Unit),
	chk_arg_types(Unit, Context, Args, Types)
	;
	is_open_type(Type, Unit, _OpenKind), !,
	%writeln(open:Term:Type:Context),
	insert_context(Term:Type, Context, Unit)
	%;
	%not(generator(Term, _, Unit)),
	%insert_context(Term:undefined_type, Context, Unit)
         ).

chk_arg_types(_Unit, dl(Context,Context), [], []).
chk_arg_types(Unit, dl(Context1, Context2),[A| Args], [T|Types]):-
       setof(C, chk_type(Unit, dl(Context1, C), A, T), CC),
       member(Context, CC),
       chk_arg_types(Unit, dl(Context, Context2), Args, Types).

local_pred(is_open_type(is_type, atom)).
is_open_type(Type, Unit, open) :-!,
	not(var(Type)),!,
	type_gen(open, Type, Unit).
is_open_type(Type, Unit, generic) :-!,
	not(var(Type)),!,
	type_gen(generic, Type, Unit).
is_open_type(Type, Unit, generic) :-!,
	not(var(Type)),!,
	type_gen(imported(_), Type, Unit).

local_pred(generator(any, is_type, atom)).
generator(Term, Type, Unit) :-
	%ground(Term),
	type_gen(call(U,Type), Type, Unit),
	catch(U:call(Type, Term),_,fail), !.
generator(Term, Type, Unit) :-
	type_gen(gen(Term, Args, _Types), Type, Unit),
	Args \== [Term].

local_pred(insert_context(typed_var, context, atom)).
insert_context(Var:Type, dl([], [Var:Type]), _).
insert_context(Var:Type, dl([X:T|Context], [X:T1|Context]), U) :-
	Var==X,!,
	merge_types(U,Type,T,T1).
insert_context(Var:Type, dl([W|Context],[W|Context1]), U) :-
       insert_context(Var:Type, dl(Context, Context1),U).

merge_types(_U, T, T, T) :- !.
merge_types(_U, T1, T2, T1) :-
	T2==any,!.
merge_types(_U,T1,T2,T2) :-
	T1==any,!.
merge_types(U, T1,T2,T3) :-
	catch((functor(T1, K, N), functor(T2,K,N)), _, fail), !,
	T1 =.. [K|TT1],
	T2 =.. [K|TT2],
	maplist(merge_types(U), TT1, TT2, TT3),
	T3 =.. [K|TT3].
merge_types(_U, T1,T1,T1) :- !.
merge_types(U, T1,T2,T3) :-
	sub_type_generator(T3,T1,U),
	sub_type_generator(T3,T2,U).


%=======================================================================
%    CHECKING ATOMIC FORMULAS

check_atomic(clause(Unit,K,Head,Body), dl(C1,C2), U:B) :-
	check_qualifier(U), !,
	check_atomic(clause(Unit,K,Head,Body), dl(C1,C2), B).

check_atomic(clause(Unit,K,Head,Body), dl(C1,C2), B) :-
	not(pred_gen(pred(B, _, _), Unit)),!,
	C2=C1,
	assert_unit_warning(open(B), clause(Unit,K,Head,Body))
	;
	pred_gen(pred(B, Args, Types), Unit),
	check_arity(clause(Unit,K,Head,Body), dl(C1,C2), B, Args, Types).


check_qualifier(_).

pred(clause_error(unit_error, error_context)).
:- dynamic(clause_error/2).

check_arity(clause(Unit,K,Head,Body), dl(C1,C2), B, Args, Types) :-
	setof(CX,
	      chk_arg_types(Unit,dl(C1,CX), Args, Types),
	     Cnts),!,
	member(C2, Cnts)
	;
	C2=C1,
	assert(clause_error(clause_type_error(C1, B, Args, Types),

			    clause(Unit,K,Head,Body))),
	fail.




pred(checked(atom,numbered_checked_clause)).
%  checked(Unit, K:CheckedClause):   Kth clause has been checked
%  and CheckedClause is the result; DYNAMIC.   Possible results:
%  -  K:clause(ContextList, Head, Body)    Head:-Body correct wrt
%     each context in ContextList
%  - K:wrong_clause(Head, Body)   Head:-Body is wrong, errors stored
:- dynamic(checked/2).

pred(check_clauses(atom)).

check_clauses(Unit) :-
	retractall(checked(_,_)),
	forall(( unit_clause(K, Unit, Head, Body),
		 pred_gen(pred(Head,_,_), Unit)),
	       check_clause(K,Unit, Head, Body) ).


ccl(Unit, K) :-
	unit_clause(K, Unit, Head, Body),
	check_clause(K, Unit, Head, Body).

check_clause(K, Unit, Head, Body) :-
	retractall(clause_error(_,_)),
	(
	setof(Cnt, check_clause(K, Unit, dl([],Cnt), Head, Body), Contexts),
	forall(clause_error(Err, ErrCnt),
	       assert_unit_warning(Err, ErrCnt)),
	assertz(checked(Unit, K:clause(Contexts, Head, Body)))
	;
	forall(clause_error(Err, ErrCnt),
	       assert_unit_error(Err, ErrCnt)),
	assertz(checked(Unit, K:wrong_clause(Head, Body)))
	).


pred(wrong_clause(numbered_checked_clause)).
wrong_clause(_:wrong_clause(_,_)).

pred(multiple_context_clause(numbered_checked_clause)).
multiple_context_clause(_:clause([_,_|_], _, _)).

pred(open_context_clause(numbered_checked_clause, list(any))).
open_context_clause(_:clause(CntList, _, _), L) :-
	setof(Term:Type, open_in(CntList,Term:Type), L).
open_in(CntList,Term:Type) :-
	member(C, CntList),
	member(Term:Type, C),
	not(var(Term)).

check_clause(K, Unit,dl([],Context), Head, Body) :-
	check_atomic(clause(Unit, K, Head, Body), dl([],Context0), Head),
	check_body(clause(Unit, K, Head, Body), dl(Context0,Context), Body).

% check_body(clause(units, 33,_,_), dl([],U), forall(pred_gen(_G3262,
% _G3242), maplist(write, [' ', _G3262, '\n']))).

check_body(_, dl(C,C), true) :- !.
check_body(_, dl(C,C), !) :- !.
check_body(_, dl(C,C), fail) :- !.
check_body(Clause, dl(C1,C2),(Bd1,Bd2)) :-!,
	check_body(Clause, dl(C1,C),Bd1),
	check_body(Clause, dl(C,C2),Bd2).
check_body(Clause,dl(C1,C2),(Bd1->Bd2)) :-!,
	check_body(Clause,dl(C1,C),Bd1),
	check_body(Clause,dl(C,C2),Bd2).
check_body(Clause,dl(C1,C2),(Bd1*->Bd2)) :-!,
	check_body(Clause,dl(C1,C),Bd1),
	check_body(Clause,dl(C,C2),Bd2).
check_body(Clause,dl(C1,C2),(Bd1;Bd2)) :-!,
	rename_vars(C1, Bd1, RBd1),
	check_body(Clause,dl(C1,C),RBd1),
	rename_vars(C1, Bd2, RBd2),
	check_body(Clause,dl(C,C2),RBd2).
check_body(Clause,dl(C1,C2),forall(Bd1,Bd2)) :-!,
	check_body(Clause,dl(C1,C),Bd1),
	check_body(Clause,dl(C,C2),Bd2).
check_body(Clause,dl(C1,C2),not(Bd)) :-!,
	rename_vars(C1, Bd, RBd),!,
	check_body(Clause,dl(C1,C2),RBd).
check_body(Clause, dl(C1,C2), once(Bd1)) :- !,
	check_body(Clause, dl(C1,C2), Bd1).
check_body(Clause, dl(C1,C2), catch(Bd1, Err, Bd2)) :- !,
	check_body(Clause, dl(C1,C), Bd1),
	check_error_msg(Clause, C, Err),
	check_body(Clause, dl(C,C2), Bd2).
check_body(Clause, dl(C1,C2), asserta(A)) :- !,
	check_assert_retract(Clause, dl(C1,C2), A).
check_body(Clause, dl(C1,C2), assertz(A)) :- !,
	check_assert_retract(Clause, dl(C1,C2), A).
check_body(Clause, dl(C1,C2), assert(A)) :- !,
	check_assert_retract(Clause, dl(C1,C2), A).
check_body(Clause, dl(C1,C2), retract(A)) :- !,
	check_assert_retract(Clause, dl(C1,C2), A).
check_body(Clause, dl(C1,C2), retractall(A)) :- !,
	check_assert_retract(Clause, dl(C1,C2), A).
check_body(clause(Unit, K, Head, Body), dl(C1,C2), setof(Term, Bd, Set)) :- !,
	 check_body(clause(Unit, K, Head, Body), dl(C1,C), Bd),
	 chk_arg_types(Unit, dl(C,C2), [Term, Set], [Type, list(Type)]).
check_body(Clause, dl(C1,C2), forall(Bd1, Bd2)) :- !,
	 check_body(Clause, dl(C1,C), Bd1),
	 check_body(Clause, dl(C,C2), Bd2).
check_body(clause(Unit, K, Head, Body), dl(C1,C2), maplist(Pred,List)) :- !,
	 chk_type(Unit, dl(C1,C), List, list(T)),!,
	 call_pred(clause(Unit, K, Head, Body), Pred, [T], dl(C,C2)).
check_body(clause(Unit, K, Head, Body), dl(C1,C2), maplist(Pred,List1,List2)) :- !,
	 chk_arg_types(Unit, dl(C1,C), [List1, List2], [list(T1), list(T2)]),
	 call_pred(clause(Unit, K, Head, Body), Pred, [T1,T2], dl(C,C2)).
check_body(_Clause, dl(C,C), CALL) :-
	 CALL =.. [call|_], !.  %call ignored
check_body(Clause, dl(C1,C2), At) :-
	check_atomic(Clause, dl(C1,C2), At).


call_pred(_, Pred, L, dl(C, [Pred:call(L)|C])) :-
	var(Pred), !.
call_pred(clause(Unit, K, Head, Body),Pred, L, dl(C1,C2)) :-
	Pred =.. [P|Args],
	same_length(AA, L),
	append(Args, AA, ArgsL),
	PredL=..[P|ArgsL],
	add_vars(AA, L, C1,C),
	check_atomic(clause(Unit, K, Head, Body), dl(C,C2), PredL).

add_vars([],[],C,C).
add_vars([X|XX], [T|TT], C1, [X:T|C2]) :-
	add_vars(XX, TT, C1, C2).



check_error_msg(_,_,_).

rename_var(V/W, V1, W) :-
	V1==V, !.
rename_var(_, V, V) :-
	var(V),!.
rename_var(S, Term, RTerm) :-
	Term =.. [F|Args],
	maplist(rename_var(S), Args, RArgs),
	RTerm =.. [F|RArgs].
rename_vars(C, Term1, Term2) :-
	term_variables(Term1, Vars),
	rename_non_context_vars(C, Vars, Term1, Term2).

rename_non_context_vars(_C, [], Term, Term).
rename_non_context_vars(C, [V|Vars], Term1, Term2) :-
	context_var(V,C), !,
	rename_non_context_vars(C, Vars, Term1, Term2).
rename_non_context_vars(C, [V|Vars], Term1, Term2) :-
	rename_var(V/_, Term1, Term), !,
	rename_non_context_vars(C, Vars, Term, Term2).


check_assert_retract(U, dl(C1, C2), (A:-B)) :-
	check_clause(asserted, U, dl(C1,C2), (A :- B)).
check_assert_retract(U, dl(C1, C2), A) :-
	not(A=(_ :- _)),
	check_atomic(U, dl(C1,C2), A).



%===============================================================

tc_clause(K,Unit, Cn:(Hd :- Bd)) :-
       unit_clause(K, Unit, Hd, Bd),
       pred_gen(pred(Hd,_,_), Unit),!,
       check_clause(K, Unit,dl([],Cn), Hd, Bd).













