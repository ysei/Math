/*
  Copyright 2011 Jose Sebastian Reguera Candal

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
:- module(prop, []).


%% prop(Proposition)
%
% True if Proposition is a well formed propositional formula.

prop(eq(P, Q)) :-
        prop(P),
        prop(Q).
prop(imp(P, Q)) :-
        prop(P),
        prop(Q).
prop(and(P, Q)) :-
        prop(P),
        prop(Q).
prop(or(P, Q)) :-
        prop(P),
        prop(Q).
prop(not(P)) :-
        prop(P).
prop(var(P)) :-
        atom(P).
prop(0).
prop(1).


%% vars(+Prop, -Vars)
%
% Vars is a sorted list of the propositional variables in Prop.

vars(eq(P, Q), V) :-
        vars(P, VP),
        vars(Q, VQ),
        ord_union(VP, VQ, V).
vars(imp(P, Q), V) :-
        vars(P, VP),
        vars(Q, VQ),
        ord_union(VP, VQ, V).
vars(and(P, Q), V) :-
        vars(P, VP),
        vars(Q, VQ),
        ord_union(VP, VQ, V).
vars(or(P, Q), V) :-
        vars(P, VP),
        vars(Q, VQ),
        ord_union(VP, VQ, V).
vars(not(P), V) :-
        vars(P, V).
vars(var(P), V) :-
        ord_empty(V0),
        ord_add_element(V0, P, V).
vars(0, V) :-
        ord_empty(V).
vars(1, V) :-
        ord_empty(V).


%% eval(+Proposition, +Environment, -Value)
%
% Evaluate the Proposition in Environmnent giving Value as a result.
% - Proposition is as defined in the predicate "prop" above.
% - Environment is a list of pairs where the first element of the pair is
% a propositional variable name and the second its associated value (0 or 1).
% For example: [p-1, q-0]
% - Value is the resulting value (0 or 1).

eval(eq(P, Q), Env, V) :-
        eval(P, Env, PV),
        eval(Q, Env, QV),
        eval_eq(PV, QV, V).        
eval(imp(P, Q), Env, V) :-
        eval(P, Env, PV),
        eval(Q, Env, QV),
        eval_imp(PV, QV, V).
eval(and(P, Q), Env, V) :-
        eval(P, Env, PV),
        eval(Q, Env, QV),
        eval_and(PV, QV, V).
eval(or(P, Q), Env, V) :-
        eval(P, Env, PV),
        eval(Q, Env, QV),
        eval_or(PV, QV, V).
eval(not(P), Env, V) :-
        eval(P, Env, PV),
        eval_not(PV, V).
eval(var(P), Env, V) :-
        memberchk(P-V, Env).
eval(0, _, 0).
eval(1, _, 1).


% eval_op(+P, +Q, -V)
% eval_op(+P, -V)
%
% Helper functions that use cuts for avoiding the indeterminacy that would
% result for using the op tables directly.

eval_eq(P, Q, V) :-
        eq(P, Q, V),
        !.
eval_imp(P, Q, V) :-
        imp(P, Q, V),
        !.
eval_and(P, Q, V) :-
        and(P, Q, V),
        !.
eval_or(P, Q, V) :-
        or(P, Q, V),
        !.
eval_not(P, V) :-
        not(P, V),
        !.


% op(P, Q, V)
% op(P, V)
%
% Truth tables for the logical operators.

eq(0, 0, 1).
eq(0, 1, 0).
eq(1, 0, 0).
eq(1, 1, 1).

imp(0, 0, 1).
imp(0, 1, 1).
imp(1, 0, 0).
imp(1, 1, 1).

and(0, 0, 0).
and(0, 1, 0).
and(1, 0, 0).
and(1, 1, 1).

or(0, 0, 0).
or(0, 1, 1).
or(1, 0, 1).
or(1, 1, 1).

not(0, 1).
not(1, 0).


:- begin_tests(prop).

test(or) :-
        eval(or(var(p), var(q)), [p-1, q-0], 1).

:- end_tests(prop).
