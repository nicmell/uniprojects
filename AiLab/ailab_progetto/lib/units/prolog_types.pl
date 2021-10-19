:- module(prolog_types, [prolog_type/1,
		         prolog_pred/1,
			 is_prolog_type/1]).

:- include('../unit_checker').

unit(prolog_types,[]).

:- use_module('../units',[]).
import_type(units,[symbol]).


pred(prolog_type(any)).
pred(prolog_pred(any)).
pred(is_prolog_type(symbol)).



is_prolog_type(T/N) :-
	prolog_type(_:Type),
	functor(Type, T, N).


prolog_type([
    inf,
    call,
    {code},
    -integer,
    +integer,
    integer + integer,
    integer * integer,
    integer - integer,
    integer div integer,
    integer mod integer,
    abs(integer)]:integer).

prolog_type([
    inf,
    call,
    -float,
    +float,
    float + float,
    float + integer,
    integer + float,
    float * float,
    float * integer,
    integer * float,
    float - float,
    float - integer,
    integer - float,
    float / float,
    float / integer,
    integer / float,
    integer / integer,
    abs(float)]:float).

prolog_type([read, write]:opening_mode).

prolog_type([{integer}, {float},
	    +number,
	    -number,
	    number+number,
	    number*number,
	    number-number,
	    number/number,
	    abs(number)]:number).

prolog_type([[], [T|list(T)]]:list(T)).

prolog_type([call]:is_stream).

prolog_type([call]:var).

prolog_type([call]:atom).

prolog_type([call]:char).

prolog_type([call]:code).

prolog_type([call]:string).

%prolog_type(_:dcg_type).

char(C) :-
	catch(char_code(C,_), _, fail).
code(C) :-
	catch(char_code(_,C), _, fail).


prolog_pred(true).
prolog_pred(!).
prolog_pred(fail).
prolog_pred(abort).

prolog_pred(var(any)).
prolog_pred(atom(any)).
prolog_pred(number(any)).
prolog_pred(integer(any)).
prolog_pred(float(any)).

prolog_pred(any = any).
prolog_pred(any \= any).
prolog_pred(any == any).
prolog_pred(any \== any).
prolog_pred(number < number).
prolog_pred(number > number).
prolog_pred(number =< number).
prolog_pred(number >= number).
prolog_pred(number =:= number).
prolog_pred(number =\= number).
prolog_pred(number is number).
prolog_pred(between(integer, integer, integer)).
prolog_pred(is_list(any)).

prolog_pred(any @< any).
prolog_pred(any @> any).
prolog_pred(any @=< any).
prolog_pred(any @>= any).
prolog_pred(any =@= any).

prolog_pred(read(any)).
prolog_pred(readln(any)).
prolog_pred(read(is_stream, any)).
prolog_pred(write(any)).
prolog_pred(writeln(any)).
prolog_pred(write(is_stream, any)).
prolog_pred(open(atom, opening_mode, is_stream)).
prolog_pred(close(is_stream)).
prolog_pred(see(atom)).
prolog_pred(seen).
prolog_pred(tell(atom)).
prolog_pred(told).

prolog_pred(member(T, list(T))).
prolog_pred(length(list(_), integer)).
prolog_pred(same_length(list(_), list(_))).
prolog_pred(sort(list(T), list(T))).


prolog_pred(any =.. list(any)).
prolog_pred(functor(any, any, integer)).
prolog_pred(term_variables(any, list(var))).

prolog_pred(nb_getval(atom, any)).
prolog_pred(nb_setval(atom, any)).

prolog_pred(module_property(atom, any)).
prolog_pred(string_codes(string, list(code))).


prolog_pred(phrase(dcg_type, list(code), list(code))).
prolog_pred(phrase(dcg_type, list(code))).






